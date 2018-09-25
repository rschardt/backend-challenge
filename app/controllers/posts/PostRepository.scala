package controllers.posts

import com.google.inject.Inject
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * A "persistance" layer for the [[Post]]
  */
class PostRepository @Inject()(
                                implicit val executionContext: ExecutionContext
                              ) {

  /**
    * This is the place where all posts are stored. You may change the type, but stick to solution form the
    * scala-std-library.
    */
  private val posts: ListBuffer[Post] = ListBuffer(
    Post(1, "Title 1", "Body 1"),
    Post(2, "Title 2", "Body 2")
  )

  def find(id: Int): Future[Option[Post]] = {
    Future {
      posts.find(_.id == id)    
    }
  }

  def findAll: Future[Seq[Post]] = {
    Future {
      posts.sortBy(post => post.id)
    }
  }

  def insert(post: Post): Future[Post] = {
    Future {

      if (false == posts.exists(x => x.id == post.id)) {
        posts += post
        post

      } else {
        throw new Exception
      }
    } 
  }

  def update(post: Post): Future[Post] = {
    Future {

      posts.find(_.id == post.id) match {
        case Some(element) => 
          posts -= element
          posts += post
          post

        case None => throw new Exception
      }
    }
  } 

  def delete(id: Int): Future[Post] = {
    Future {
      posts.find(_.id == id) match {
        case Some(element) => 
          posts -= element
          element

        case None => throw new Exception
      }
    }
  } 
}
