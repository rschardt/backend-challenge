package controllers.posts

import javax.inject.Inject
import play.api.libs.json.{JsError, JsValue, Json}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Result, Request}
import scala.concurrent.{ExecutionContext, Future}

class PostsController @Inject()(
                                 cc: ControllerComponents,
                                 postRepository: PostRepository,
                                 implicit val executionContext: ExecutionContext
                               ) extends AbstractController(cc) {

  val idAlreadyUsedMsg = "Id is already in use";
  val postNotFoundMsg = "Post not found";


  /**
    * Helper to create Results
    * TODO this Function could be refactored into a Factory
    * TODO i know 
    */
  def createResult(status: Int, message: Option[String], data: Option[JsValue]) : Result = {

    val statusResult = Json.toJson(StatusResult(status, message, data));
    status match {

      case OK => Ok(statusResult)
      case NOT_FOUND => NotFound(statusResult)
      case BAD_REQUEST => BadRequest(statusResult)
      case _ => BadRequest(statusResult)
    }
  }

  /**
    * This takes a Json in the format of
    *
    * {
    *   "id": 1,
    *   "title": "My Title",
    *   "body": "My Post"
    * }
    *
    * and saves it into the persistance layer. The created Post is then returned.
    *
    * TODO: It should fail, if there is already a Post with the same id present.
    *
    */
  def create(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    val postResult = request.body.validate[Post]
    postResult.fold(
      errors => {
        Future {
          createResult(BAD_REQUEST, Some(JsError.toJson(errors).toString()), None)
        }
      },

      post => {
        postRepository.insert(post).map {
          persisted => createResult(OK, None, Some(Json.toJson(persisted)))

        } recover {
          case e:Exception => 
            createResult(BAD_REQUEST, Some(idAlreadyUsedMsg), None)
        }
      })
    }


  /**
    * This returns a Json Array with a list of all Posts.
    *
    * TODO: Should return the Posts in ascending order on the ids.
    */
  def readAll(): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.findAll.map { posts =>
      createResult(OK, None, Some(Json.toJson(posts)))
    }
  }

  /**
    * TODO: Returns only the post with the matching id
    * TODO: If the post does not exist, returns a 404 with a json like
    *
    * {
    *   "status": 404,
    *   "message": "Post not found"
    * }
    *
    */
  def readSingle(id: Int): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.find(id).map { 
      case Some(post) => createResult(OK, None, Some(Json.toJson(post)))
      case None => createResult(NOT_FOUND,  Some(postNotFoundMsg), None)
    }
  }

  /**
    * Does not contain any body in the request
    *
    * TODO Deletes the post with the given id.
    */
  def delete(id: Int): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>

    postRepository.delete(id).map { 
      post => createResult(OK, None, Some(Json.toJson(post)))

    } recover {
      case e:Exception => 
        createResult(NOT_FOUND,  Some(postNotFoundMsg), None)
    }
  }

  /**
    * Request body contains the post.
    *
    * TODO Updates the post with the given id.
    * TODO Changing the id of a post must not possible.
    */
  def update(id: Int): Action[JsValue] = Action.async(parse.json) { implicit request =>

    val postResult = request.body.validate[Post]
    postResult.fold(
      errors => {
        Future {
          createResult(BAD_REQUEST, Some(JsError.toJson(errors).toString()), None)
        }
      },
      
      post => {

        val modifiedPost = Post(id, post.title, post.body)
        postRepository.update(modifiedPost).map {
          persisted => createResult(OK, None, Some(Json.toJson(modifiedPost)))

        } recover {
          case e:Exception => 
            createResult(NOT_FOUND,  Some(postNotFoundMsg), None)
        }
    }) 
  }
}
