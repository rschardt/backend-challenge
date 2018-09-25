package controllers.posts

import play.api.libs.json.{Format, Json}

/**
  * Represents a single post
  */
case class Post(
               id: Int,
               title: String,
               body: String
               )

object Post {

  implicit val postFormat: Format[Post] = Json.format[Post]

}
