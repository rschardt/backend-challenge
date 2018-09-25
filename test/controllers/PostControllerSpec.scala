package controllers.post

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json.{JsError, JsValue, Json}

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class PostControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  // General Posts
  var testPost = Json.obj("id" -> 123, "title" -> "Test CREATE Post", "body" -> "created for a test");
  var updatedPost = Json.obj("id" -> 124, "title" -> "Test UPDATE Post", "body" -> "this post is updated");
  var differentPost = Json.obj("id" -> 125, "title" -> "Test UPDATE Post", "body" -> "this post can't be found");

  // Error-Messages
  var idExistsMsg = Json.obj("status" -> 400, "message" -> "Id is already in use" );
  var notFoundMsg = Json.obj("status" -> 404, "message" -> "Post not found" );


  "CREATE POST" should {

    "return a 400-Error when trying to create the same element twice" in {
      var request = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      var createPost = route(app, request).get

      status(createPost) mustBe OK
      contentType(createPost) mustBe Some("application/json")
      contentAsString(createPost) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)
    
      var createPost2 = route(app, request).get
      status(createPost2) mustBe BAD_REQUEST
      contentType(createPost2) mustBe Some("application/json")
      contentAsString(createPost2) mustBe (idExistsMsg.toString)
    }
  }


  "READALL GET" should {

    "return all elements sorted by id in ascending order" in {

      var element1 = Json.obj("id" -> 500, "title" -> "Test UPDATE Post", "body" -> "this post has the biggest id");
      var element2 = Json.obj("id" -> 3, "title" -> "Test UPDATE Post", "body" -> "this post is not special");

      // Create Elements in wrong Order
      var createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](element1);
      var readAll = route(app, createRequest).get
      createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](element2);
      readAll = route(app, createRequest).get


      // Get all Entries and check if the ordering of the id-field is ascending
      var getAllRequest = FakeRequest(GET, "/api/v1/posts")
      readAll = route(app, getAllRequest).get

      status(readAll) mustBe OK
      contentType(readAll) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(readAll)) \\ "id")
      var lastNumber = 0;

      for (element <- idArray) {
      
        val number = element.as[Int]
        val isAscending = (lastNumber < number)

        isAscending mustBe true
        if(isAscending) lastNumber = number
      }
    }
      

    "delete all elements and try to return all" in {
      
      // Delete all Standard-Entries
      var deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "1");
      var readAll = route(app, deleteRequest).get

      deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "2");
      readAll = route(app, deleteRequest).get

      status(readAll) mustBe OK
      contentType(readAll) mustBe Some("application/json")

      // Get all Entries and check if the ordering of the id-field is ascending
      var getAllRequest = FakeRequest(GET, "/api/v1/posts")
      readAll = route(app, getAllRequest).get

      status(readAll) mustBe OK
      contentType(readAll) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(readAll)) \\ "id")
      (idArray.isEmpty) mustBe true
    }
  }


  "READSINGLE GET" should {
    "return the post with the matching id" in {

      // Create Test-Entry
      val createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      var readSingle = route(app, createRequest).get

      // Get Single
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      readSingle = route(app, getSingleRequest).get

      status(readSingle) mustBe OK
      contentType(readSingle) mustBe Some("application/json")
      contentAsString(readSingle) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in { 
      val request = FakeRequest(GET, "/api/v1/posts/" + "124")
      val readSingle = route(app, request).get

      status(readSingle) mustBe NOT_FOUND
      contentType(readSingle) mustBe Some("application/json")
      contentAsString(readSingle) mustBe (notFoundMsg.toString)
    }
  }


  "UPDATE PUT" should {

    "change the post with the given id, altering the id shouldn't be possible" in {

      // updatedPost has id 124 instead of 123, when compared with the validationPost it should prove that the id hasn't changed
      var validationPost = Json.obj("id" -> 123, "title" -> "Test UPDATE Post", "body" -> "this post is updated");

      // Create Test-Entry
      val createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      var put = route(app, createRequest).get

      // Update
      val putRequest = FakeRequest(PUT, "/api/v1/posts/" + "123").withHeaders("Content-type" -> "application/json").withBody[JsValue](updatedPost);
      put = route(app, putRequest).get

      status(put) mustBe OK
      contentType(put) mustBe Some("application/json")
      contentAsString(put) mustBe (Json.obj("status" -> 200, "data" -> validationPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val request = FakeRequest(PUT, "/api/v1/posts/" + "200").withHeaders("Content-type" -> "application/json").withBody[JsValue](differentPost);
      val put = route(app, request).get

      status(put) mustBe NOT_FOUND
      contentType(put) mustBe Some("application/json")
      contentAsString(put) mustBe (notFoundMsg.toString)
    }
  }


  "DELETE (DELETE)" should {
    "delete the right post and try to find it after the deletion" in {

      // Create Test-Entry
      val createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      var delete = route(app, createRequest).get

      // Delete the Test-Entry
      val deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "123");
      delete = route(app, deleteRequest).get

      status(delete) mustBe OK
      contentType(delete) mustBe Some("application/json")
      contentAsString(delete) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)

      // Try to find the Test-Entry
      val findRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      delete = route(app, findRequest).get

      status(delete) mustBe NOT_FOUND
      contentType(delete) mustBe Some("application/json")
      contentAsString(delete) mustBe (notFoundMsg.toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val request = FakeRequest(DELETE, "/api/v1/posts/" + "500")
      val home = route(app, request).get

      status(home) mustBe NOT_FOUND
      contentType(home) mustBe Some("application/json")
      contentAsString(home) mustBe (notFoundMsg.toString)
    }
  }
}
