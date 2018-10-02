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
  val idExistsMsg = Json.obj("status" -> BAD_REQUEST, "message" -> "Id is already in use" );
  val notFoundMsg = Json.obj("status" -> NOT_FOUND, "message" -> "Post not found" );

  // Helper-Functions  
  def tryToGetResult(futureResult: Option[scala.concurrent.Future[play.api.mvc.Result]]) = futureResult match {
    case Some(x) => x
    case None => throw new Exception("Server didn't responded with a Result")
  }

  "CREATE POST" should {

    "return a 400-Error when trying to create the same element twice" in {
      val postRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult1 = tryToGetResult(route(app, postRequest));

      status(postResult1) mustBe OK
      contentType(postResult1) mustBe Some("application/json")
      contentAsString(postResult1) mustBe (Json.obj("status" -> OK, "data" -> testPost).toString)
    
      val postResult2 = tryToGetResult(route(app, postRequest))
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
      val postResult1 = tryToGetResult(route(app, postRequest1))

      val postRequest2 = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](element2);
      val postResult2 = tryToGetResult(route(app, postRequest2))

      // Get all Entries and check if the ordering of the id-field is ascending
      val getAllRequest = FakeRequest(GET, "/api/v1/posts")
      val getAllResult = tryToGetResult(route(app, getAllRequest))

      status(getAllResult) mustBe OK
      contentType(getAllResult) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(getAllResult)) \\ "id")
      val isAscending : Boolean = (idArray.foldLeft(true, 0) { (prev, next) =>

        val number = next.as[Int]
        if (prev._2 > number) {
          (false, 2147483647) // 2147483647 is the max value an Int in Scala can have
        } else {
          (true, number)
        }
      })._1

      isAscending mustBe true
    }
      

    "delete all elements and try to return all" in {
      
      // Delete all Standard-Entries
      val deleteRequest1 = FakeRequest(DELETE, "/api/v1/posts/" + "1");
      val deleteResult1 = tryToGetResult(route(app, deleteRequest1))

      status(deleteResult1) mustBe OK
      contentType(deleteResult1) mustBe Some("application/json")

      val deleteRequest2 = FakeRequest(DELETE, "/api/v1/posts/" + "2");
      val deleteResult2 = tryToGetResult(route(app, deleteRequest2))

      status(deleteResult2) mustBe OK
      contentType(deleteResult2) mustBe Some("application/json")

      // Get all Entries and check if the ordering of the id-field is ascending
      val getAllRequest = FakeRequest(GET, "/api/v1/posts")
      val getAllResult = tryToGetResult(route(app, getAllRequest))

      status(getAllResult) mustBe OK
      contentType(getAllResult) mustBe Some("application/json")
      
      val idArray = (Json.parse(contentAsString(getAllResult)) \\ "id")
      (idArray.isEmpty) mustBe true
    }
  }


  "READSINGLE GET" should {
    "return the post with the matching id" in {

      // Create Test-Entry
      val postRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult = tryToGetResult(route(app, postRequest))

      // Get Single
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      val readSingle = tryToGetResult(route(app, getSingleRequest))

      status(readSingle) mustBe OK
      contentType(readSingle) mustBe Some("application/json")
      contentAsString(readSingle) mustBe (Json.obj("status" -> OK, "data" -> testPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in { 
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "124")
      val getSingleResult = tryToGetResult(route(app, getSingleRequest))

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
      val postResult = tryToGetResult(route(app, postRequest))

      status(postResult) mustBe OK
      contentType(postResult) mustBe Some("application/json")
      contentAsString(postResult) mustBe (Json.obj("status" -> OK, "data" -> testPost).toString)

      // Update
      val putRequest = FakeRequest(PUT, "/api/v1/posts/" + "123").withHeaders("Content-type" -> "application/json").withBody[JsValue](updatedPost);
      val putResult = tryToGetResult(route(app, putRequest))

      status(putResult) mustBe OK
      contentType(putResult) mustBe Some("application/json")
      contentAsString(putResult) mustBe (Json.obj("status" -> OK, "data" -> validationPost).toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val putRequest = FakeRequest(PUT, "/api/v1/posts/" + "259").withHeaders("Content-type" -> "application/json").withBody[JsValue](differentPost);
      val putResult = tryToGetResult(route(app, putRequest))

      status(putResult) mustBe NOT_FOUND
      contentType(putResult) mustBe Some("application/json")
      contentAsString(putResult) mustBe (notFoundMsg.toString)
    }
  }


  "DELETE (DELETE)" should {
    "delete the right post and try to find it after the deletion" in {

      // Create Test-Entry
      val postRequest = FakeRequest(POST, "/api/v1/posts").withHeaders("Content-type" -> "application/json").withBody[JsValue](testPost);
      val postResult = tryToGetResult(route(app, postRequest))

      // Delete the Test-Entry
      val deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "123");
      val deleteResult = tryToGetResult(route(app, deleteRequest))

      status(deleteResult) mustBe OK
      contentType(deleteResult) mustBe Some("application/json")
      contentAsString(deleteResult) mustBe (Json.obj("status" -> OK, "data" -> testPost).toString)

      // Try to find the Test-Entry
      val getSingleRequest = FakeRequest(GET, "/api/v1/posts/" + "123")
      val getSingleResult = tryToGetResult(route(app, getSingleRequest))

      status(getSingleResult) mustBe NOT_FOUND
      contentType(getSingleResult) mustBe Some("application/json")
      contentAsString(getSingleResult) mustBe (notFoundMsg.toString)
    }

    "return a 404-Error, because the post doesn't exist" in {
      val deleteRequest = FakeRequest(DELETE, "/api/v1/posts/" + "500")
      val deleteResult = tryToGetResult(route(app, deleteRequest))

      status(deleteResult) mustBe NOT_FOUND
      contentType(deleteResult) mustBe Some("application/json")
      contentAsString(deleteResult) mustBe (notFoundMsg.toString)
    }
  }
}
