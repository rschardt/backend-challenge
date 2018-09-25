package controllers.posts

import javax.inject.Inject
import play.api.libs.json.{JsError, JsValue, Json}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class PostsController @Inject()(
                                 cc: ControllerComponents,
                                 postRepository: PostRepository,
                                 implicit val executionContext: ExecutionContext
                               ) extends AbstractController(cc) {

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
          BadRequest(Json.obj("status" -> 400, "message" -> JsError.toJson(errors)))
        }
      },
      post => {
        postRepository.insert(post).map {
          persisted => Ok(Json.obj("status" -> 200, "data" -> Json.toJson(persisted)))

        } recover {
          case e:Exception => 
            BadRequest(Json.obj("status" -> 400, "message" -> "Id is already in use" ))
        }
  }) }

  /**
    * This returns a Json Array with a list of all Posts.
    *
    * TODO: Should return the Posts in ascending order on the ids.
    */
  def readAll(): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.findAll.map { posts =>
      val json = Json.obj("status" -> 200, "data" -> Json.toJson(posts))
      Ok(json)
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
      case Some(post) => Ok(Json.obj("status" -> 200, "data" -> Json.toJson(post)))
      case None => NotFound(Json.obj("status" -> 404, "message" -> "Post not found" ))
    }
  }

  /**
    * Does not contain any body in the request
    *
    * TODO Deletes the post with the given id.
    */
  def delete(id: Int): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>

    postRepository.delete(id).map { 
      post => Ok(Json.obj("status" -> 200, "data" -> Json.toJson(post)))

    } recover {
      case e:Exception => 
        NotFound(Json.obj("status" -> 404, "message" -> "Post not found" ))
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
          BadRequest(Json.obj("status" -> 400, "message" -> JsError.toJson(errors)))
        }
      },
      post => {

        val modifiedPost = Post(id, post.title, post.body)
        postRepository.update(modifiedPost).map {
          persisted => Ok(Json.obj("status" -> 200, "data" -> Json.toJson(modifiedPost)))

        } recover {
          case e:Exception => 
            NotFound(Json.obj("status" -> 404, "message" -> "Post not found" ))
        }
    }) 
  }
}
