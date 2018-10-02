package controllers.posts

import play.api.libs.json.{Format, Json, JsValue}

/**
  * Represents a single statusResult
  */
case class StatusResult(
               status: Int,
               message: Option[String],
               data: Option[JsValue]
               )

object StatusResult {

  implicit val requestFormat: Format[StatusResult] = Json.format[StatusResult]

}
