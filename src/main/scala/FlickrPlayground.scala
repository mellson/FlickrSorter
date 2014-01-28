import java.io._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import uk.co.bigbeeconsultants.http.{Config, HttpClient}
import uk.co.bigbeeconsultants.http.response.Response
import java.net.URL
import play.api.libs.json._
import java.awt.Desktop

object FlickrPlayground {
  // This value determines which date_taken will get set for photos without one in the EXIF data
  val defaultDate = new DateTime(0)

  val baseUrl = "http://api.flickr.com/services/rest/?"
  val authUrl = "http://flickr.com/services/auth/?"
  val apiKey = api_key(FlickrKeys.apikey)
  val secret = FlickrKeys.secret
  val json = format("json")

  // Token til at bruge vores egen flickr account
  val authToken = auth_token(FlickrKeys.authtoken)
  val userId = user_id(FlickrKeys.userid)

  trait FlickrOption {
    def id: String

    def optionName = s"${this.toString.split('(').head}"

    def optionNameAndValue = s"${this.toString.split('(').head}$id"

    def flickrRequestName = s"&${this.toString.split('(').head}=$id"
  }

  case class photo_id(id: String) extends FlickrOption

  case class user_id(id: String) extends FlickrOption

  case class api_key(id: String) extends FlickrOption

  case class format(id: String) extends FlickrOption

  case class api_sig(id: String) extends FlickrOption

  case class method(id: String) extends FlickrOption

  case class auth_token(id: String) extends FlickrOption

  case class frob(id: String) extends FlickrOption

  case class perms(id: String) extends FlickrOption

  case class extras(id: String) extends FlickrOption

  case class per_page(id: String) extends FlickrOption

  case class page(id: String) extends FlickrOption

  case class date_posted(id: String) extends FlickrOption

  case class date_uploaded(id: String) extends FlickrOption

  case object authorizeCall extends FlickrOption {
    def id: String = "authorize this call please"
  }


  def flickrRequest(params: FlickrOption*): String = {
    def helper(params2: List[FlickrOption], authorize: Boolean): String = params2 match {
      // The base url of all rest requests to Flickr along with the API Key in request form
      case Nil => if (authorize) authUrl + apiKey.flickrRequestName else baseUrl + apiKey.flickrRequestName
      case x :: xs => helper(xs, authorize) + (if (x != authorizeCall) x.flickrRequestName else "")
    }
    val listParams = params.toList
    val authorizeThisCall = listParams.contains(authorizeCall)
    // Get the result as json
    helper(listParams, authorizeThisCall) //+ json.flickrRequestName
  }

  def signedFlickrRequest(params: FlickrOption*): String = {
    // Sort all the parameters based on name and build the string needed for the signature
    val listParams = apiKey :: params.toList.filter(fo => fo != authorizeCall)
    val paramsSortedString = listParams.sortBy(_.optionName).map(_.optionNameAndValue).mkString
    flickrRequest(params: _*) + api_sig(md5Hash(secret + paramsSortedString)).flickrRequestName
  }

  // Remove the jsonFlickrApi() part from a FlickrResponse and return it as json
  def flickrJson(response: String): JsValue = Json.parse(response.substring(14, response.length - 1))

  def cleanString(s: String) = s.substring(1, s.length - 1)

  def md5Hash(text: String): String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes).map(0xFF & _).map {
    "%02x".format(_)
  }.foldLeft("") {
    _ + _
  }

  def getAuthToken: auth_token = {
    // Get login
    val httpClient = new HttpClient
    val frobRequest = signedFlickrRequest(method("flickr.auth.getFrob"), json)
    val response: Response = httpClient.get(new URL(frobRequest))

    // TODO Check at det er den rigtige kode der kommer retur fra flickr inden dette kald
    val frobString = flickrJson(response.body.toString()).\("frob").\("_content").toString()
    val frobResponse = frob(cleanString(frobString))

    val authRequest = signedFlickrRequest(authorizeCall, perms("write"), frobResponse)
    println(authRequest)
    val authResponse: Response = httpClient.get(new URL(authRequest))

    // Redirect the request to the desktop browser
    if (authResponse.request.isGet)
      if (Desktop.isDesktopSupported)
        Desktop.getDesktop.browse(authResponse.request.url.toURI)

    readLine("Press any key when you have authorized the application at Flickr.")

    val authTokenRequest = signedFlickrRequest(method("flickr.auth.getToken"), frobResponse, json)
    val authTokenResponse: Response = httpClient.get(new URL(authTokenRequest))
    val authTokenString = flickrJson(authTokenResponse.body.toString()).\("auth").\("token").\("_content").toString()
    val token = auth_token(cleanString(authTokenString))
    token
  }

  case class FlickrPhoto(title: String, id: String, dateUploaded: DateTime, datePosted: DateTime, dateTaken: DateTime, dateLastUpdated: DateTime)

  def oldestPossibleDate(): DateTime = {
    //    val config = Config(connectTimeout = 30000, readTimeout = 15000)
    //    val httpClient: HttpClient = new HttpClient(config)
    //    val dateRequest = signedFlickrRequest(method("flickr.people.getInfo"),userId,json)
    //    val dateResponse: Response = httpClient.get(new URL(dateRequest))
    //    val pageResponseString = dateResponse.body.toString()
    //    println(pageResponseString)
    //    println(flickrJson(pageResponseString).\("person").\("photos").toString())
    new DateTime(1251846722L * 1000)
  }

  def photoFromJson(jsonPhoto: JsValue): FlickrPhoto = {
    def dateFromJson(date: JsValue): DateTime = new DateTime(cleanString(date.toString()).toLong * 1000)
    def date_takenFromJson(date: JsValue): DateTime = {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
      formatter.parseDateTime(cleanString(date.toString()))
    }
    val title = cleanString(jsonPhoto.\("title").toString())
    val id = cleanString(jsonPhoto.\("id").toString())
    val dateUploaded = dateFromJson(jsonPhoto.\("dateupload"))
    // There is no datePosted when batch fetching photos - but it is initially the same as the date_upload
    //    val datePosted = dateFromJson(jsonPhoto.\("posted"))
    val dateLastUpdated = dateFromJson(jsonPhoto.\("lastupdate"))
    val dateTaken = date_takenFromJson(jsonPhoto.\("datetaken"))
    FlickrPhoto(title, id, dateUploaded, dateUploaded, dateTaken, dateLastUpdated)
  }

  def photosFromJson(json: String): List[FlickrPhoto] = {
    val js: List[JsValue] = flickrJson(json).\("photos").\("photo").as[List[JsValue]]
    js map photoFromJson
  }

  def photosWithWrongDates(photos: List[FlickrPhoto]): List[FlickrPhoto] = {
    def withinRange(p: FlickrPhoto) = {
      val photoUnixTime = p.dateTaken.toInstant.getMillis
      val oldestPossibleUnixTime = oldestPossibleDate().toInstant.getMillis
      val compareTime = if (photoUnixTime < oldestPossibleUnixTime) oldestPossibleUnixTime else photoUnixTime
      val rangeInHours = 2
      // Get difference in seconds
      val k = (p.dateUploaded.toInstant.getMillis - compareTime) / 1000
      if ((k / 3600) < rangeInHours) true
      else false
    }
    for {
      photo <- photos
      if !withinRange(photo)
    } yield photo
  }

  def correctPhotoDates(p: FlickrPhoto, httpClient: HttpClient, setOldestPossibleDate: Boolean = false) = {
    val seconds = p.dateTaken.toInstant.getMillis / 1000
    val oldest = oldestPossibleDate().toInstant.getMillis / 1000
    val newDatePosted = if (seconds < oldest || setOldestPossibleDate) oldest else seconds
    val photoDateRequest = signedFlickrRequest(method("flickr.photos.setDates"), photo_id(p.id), date_posted(newDatePosted.toString), date_uploaded(newDatePosted.toString), authToken, json)
    val response: Response = httpClient.get(new URL(photoDateRequest))
    // Check if the date was corrected
    val ok = cleanString(flickrJson(response.body.toString()).\("stat").toString()) == "ok"
    if (ok) println(s"Correctly corrected - $p")
    else {
      println("Error - " + response.body.toString())
      println(newDatePosted)
    }
  }

  def getFlickrPhotos: List[FlickrPhoto] = {
    val photosPerPage = 500.toString
    val config = Config(connectTimeout = 30000, readTimeout = 15000)
    val httpClient: HttpClient = new HttpClient(config)
    val initialRequest = signedFlickrRequest(method("flickr.people.getPhotos"), extras("date_upload,date_taken,last_update"), per_page(photosPerPage), authToken, userId, json)
    val initialResponse: Response = httpClient.get(new URL(initialRequest))
    val initialResponseString = initialResponse.body.toString()
    var photos: List[FlickrPhoto] = photosFromJson(initialResponseString)
    val numberOfPages = flickrJson(initialResponseString).\("photos").\("pages").toString().toInt
    println(s"On page 1 out of $numberOfPages.")
    for (i <- 2 to numberOfPages) {
      val pageRequest = signedFlickrRequest(method("flickr.people.getPhotos"), extras("date_upload,date_taken,last_update"), page(i.toString), per_page(photosPerPage), authToken, userId, json)
      val pageResponse: Response = httpClient.get(new URL(pageRequest))
      val pageResponseString = pageResponse.body.toString()
      val localPhotos = photosFromJson(pageResponseString)
      photos ++= localPhotos
      println(s"On page $i out of $numberOfPages.")
    }
    photos
  }

  def flickrPhotoStream(photos: List[FlickrPhoto]) = {
    println("photos " + photos.size)
    //    println("wrong photos " + wrongPhotos.size)
    //    readLine("Press any key to continue...")
    //
    //    for (i <- 1 to wrongPhotos.size) {
    //      println(s"Correcting photo $i out of ${wrongPhotos.size}.")
    //      correctPhotoDates(wrongPhotos(i - 1), httpClient)
    //    }
  }

  val serializePath = "metadataBackup/flickPhoto"
  def serializeFlickrPhotos(photos: List[FlickrPhoto]): Unit = {
    var i = 0
    for (photo <- photos) {
      val fos = new FileOutputStream(serializePath + i)
      val oos = new ObjectOutputStream(fos)
      oos.writeObject(photo)
      oos.close()
      i += 1
    }
  }

  class ObjectInputStreamWithCustomClassLoader(fileInputStream: FileInputStream) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try {
        Class.forName(desc.getName, false, getClass.getClassLoader)
      }
      catch {
        case ex: ClassNotFoundException => super.resolveClass(desc)
      }
    }
  }

  def deserializeFlickrPhotos(): List[FlickrPhoto] = {
    var photos: List[FlickrPhoto] = Nil
    var i = 0
    var f = new File(serializePath + i)
    while (f.exists() && f.canRead) {
      val fis = new FileInputStream(f)
      val ois = new ObjectInputStreamWithCustomClassLoader(fis)
      val photo: FlickrPhoto = ois.readObject().asInstanceOf[FlickrPhoto]
      photos = photo :: photos
      ois.close()
      i += 1
      f = new File(serializePath + i)
    }
    photos
  }

  def movePhotosWithNoDateToBackOfPhotoStream() = {
    def dateFromString(date: String): DateTime = {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      formatter.parseDateTime(date)
    }
    val config = Config(connectTimeout = 30000, readTimeout = 15000)
    val httpClient: HttpClient = new HttpClient(config)
    val november14 = dateFromString("2013-11-14")
    val november19 = dateFromString("2013-11-19")
    for ((b,photos) <- deserializeFlickrPhotos().groupBy(p => p.datePosted))
      for (photo <- photos)
        if (photo.dateTaken == photo.datePosted && photo.datePosted.isAfter(november14) && photo.datePosted.isBefore(november19))
          correctPhotoDates(photo, httpClient, setOldestPossibleDate = true)

  }

  def main(args: Array[String]): Unit = {
    //    println(oldestPossibleDate())
    //    flickrPhotoStream()
    movePhotosWithNoDateToBackOfPhotoStream()


    //    val httpClient = new HttpClient
    //    val photoRequest = signedFlickrRequest(method("flickr.photos.getInfo"),photo_id("666"),authToken,json)
    //
    ////    val photoRequest = signedFlickrRequest(method("flickr.people.getPhotos"),extras("date_upload,date_taken,last_update"),page("140"),authToken,userId)
    //    val response: Response = httpClient.get(new URL(photoRequest))
    //    println(response.body.toString())
    //
    //    val photo = photoFromJson(response.body.toString())
    //    println(photo.datePosted.toInstant.getMillis / 1000)
    //
    //    val now = (new DateTime().toInstant.getMillis / 1000).toString
    //    val photoDateRequest = signedFlickrRequest(method("flickr.photos.setDates"),photo_id("666"),date_posted(now),authToken,json)
    //    val response2: Response = httpClient.get(new URL(photoDateRequest))
    //    val ok = cleanString(flickrJson(response2.body.toString()).\("stat").toString()) == "ok"
    //    println(js)
  }
}
