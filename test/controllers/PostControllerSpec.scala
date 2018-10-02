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
  val testPost = Json.obj("id" -> 123, "title" -> "Test CREATE Post", "body" -> "created for a test");
  val updatedPost = Json.obj("id" -> 124, "title" -> "Test UPDATE Post", "body" -> "this post is updated");
  val differentPost = Json.obj("id" -> 125, "title" -> "Test UPDATE Post", "body" -> "this post can't be found");

  // Error-Messages
  val idExistsMsg = Json.obj("status" -> 400, "message" -> "Id is already in use" );
  val notFoundMsg = Json.obj("status" -> 404, "message" -> "Post not found" );


  "CREATE POST" should {

    "return a 400-Error when trying to create the same element twice" in {
      val postRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult1 = route(app, postRequest).get

      status(postResult1) mustBe OK
      contentType(postResult1) mustBe Some("application/json")
      contentAsString(postResult1) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)
    
      val postResult2 = route(app, postRequest).get
      status(postResult2) mustBe BAD_REQUEST
      contentType(postResult2) mustBe Some("application/json")
      contentAsString(postResult2) mustBe (idExistsMsg.toString)
    }
  }


  "READALL GET" should {

    "return all elements sorted by id in ascending order" in {

      val element1 = Json.obj("id" -> 500, "title" -> "Test UPDATE Post", "body" -> "this post has the biggest id");
      val element2 = Json.obj("id" -> 3, "title" -> "Test UPDATE Post", "body" -> "this post is not special");

      // Create Elements in wrong Order
      val postRequest1 = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](element1);
      val postResult1 = route(app, postRequest1).get

      val postRequest2 = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](element2);
      val postResult2 = route(app, postRequest2).get


      // Get all Entries and check if the ordering of the id-field is ascending
      val getAllRequest = FakeRequest(GET, "/api/v1/posts")
      val getAllResult = route(app, getAllRequest).get

      status(getAllResult) mustBe OK
      contentType(getAllResult) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(getAllResult)) \\ "id")
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
      val deleteRequest1 = FakeRequest(DELETE, "/api/v1/posts/" + "1");
      val deleteResult1 = route(app, deleteRequest1).get

      status(deleteResult1) mustBe OK
      contentType(deleteResult1) mustBe Some("application/json")

      val deleteRequest2 = FakeRequest(DELETE, "/api/v1/posts/" + "2");
      val deleteResult2 = route(app, deleteRequest2).get

      status(deleteResult2) mustBe OK
      contentType(deleteResult2) mustBe Some("application/json")

      // Get all Entries and check if the ordering of the id-field is ascending
      val getAllRequest = FakeRequest(GET, "/api/v1/posts")
      val getAllResult = route(app, getAllRequest).get

      status(getAllResult) mustBe OK
      contentType(getAllResult) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(getAllResult)) \\ "id")
      (idArray.isEmpty) mustBe true
    }
  }


  "READSINGLE GET" should {
    "return the post with the matching id" in {

      // Create Test-Entry
      val createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val createTestResult = route(app, createRequest).get

      // Get Single
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      val readSingle = route(app, getSingleRequest).get

      status(readSingle) mustBe OK
      contentType(readSingle) mustBe Some("application/json")
      contentAsString(readSingle) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in { 
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "124")
      val getSingleResult = route(app, getSingleRequest).get

      status(getSingleResult) mustBe NOT_FOUND
      contentType(getSingleResult) mustBe Some("application/json")
      contentAsString(getSingleResult) mustBe (notFoundMsg.toString)
    }
  }


  "UPDATE PUT" should {

    "change the post with the given id, altering the id shouldn't be possible" in {

      // updatedPost has id 124 instead of 123, when compared with the validationPost it should prove that the id hasn't changed
      val validationPost = Json.obj("id" -> 123, "title" -> "Test UPDATE Post", "body" -> "this post is updated");

      // Create Test-Entry
      val postRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult = route(app, postRequest).get

      status(postResult) mustBe OK
      contentType(postResult) mustBe Some("application/json")
      contentAsString(postResult) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)

      // Update
      val putRequest = FakeRequest(PUT, "/api/v1/posts/" + "123").withHeaders("Content-type" -> "application/json").withBody[JsValue](updatedPost);
      val putResult = route(app, putRequest).get

      status(putResult) mustBe OK
      contentType(putResult) mustBe Some("application/json")
      contentAsString(putResult) mustBe (Json.obj("status" -> 200, "data" -> validationPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val putRequest = FakeRequest(PUT, "/api/v1/posts/" + "200").withHeaders("Content-type" -> "application/json").withBody[JsValue](differentPost);
      val putResult = route(app, putRequest).get

      status(putResult) mustBe NOT_FOUND
      contentType(putResult) mustBe Some("application/json")
      contentAsString(putResult) mustBe (notFoundMsg.toString)
    }
  }


  "DELETE (DELETE)" should {
    "delete the right post and try to find it after the deletion" in {

      // Create Test-Entry
      val createRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult = route(app, createRequest).get

      // Delete the Test-Entry
      val deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "123");
      val deleteResult = route(app, deleteRequest).get

      status(deleteResult) mustBe OK
      contentType(deleteResult) mustBe Some("application/json")
      contentAsString(deleteResult) mustBe (Json.obj("status" -> 200, "data" -> testPost).toString)

      // Try to find the Test-Entry
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      val getSingleResult = route(app, getSingleRequest).get

      status(getSingleResult) mustBe NOT_FOUND
      contentType(getSingleResult) mustBe Some("application/json")
      contentAsString(getSingleResult) mustBe (notFoundMsg.toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "500")
      val deleteResult = route(app, deleteRequest).get

      status(deleteResult) mustBe NOT_FOUND
      contentType(deleteResult) mustBe Some("application/json")
      contentAsString(deleteResult) mustBe (notFoundMsg.toString)
    }
  }
}
