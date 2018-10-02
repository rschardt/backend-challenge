package controllers.posts

import com.google.inject.Inject
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

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

      if (posts.exists(x => x.id == post.id)) {
        throw new Exception("insert failed: post with an identical id already exists in posts"); 

      } else {
        posts += post
        post
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

        case None => throw new Exception("update failed: couldn't find a post with the specified id") 
      }
    }
  } 

  def delete(id: Int): Future[Post] = {
    Future {
      posts.find(_.id == id) match {
        case Some(element) => 
          posts -= element
          element

        case None => throw new Exception("delete failed: couldn't find a post with the specified id")
      }
    }
  } 
}
