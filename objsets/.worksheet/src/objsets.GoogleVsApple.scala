package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

}

abstract class TweetSet {

  /** This method takes a predicate and returns a subset of all the elements
   *  in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = {
	  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
	    if (accu.isEmpty){this}
	    else{
	      val h = accu.head
	      if (!p(h)) {
	        val l = accu.remove(h)
	        filter0(p, l)
	      }
	      else {
	        filter0(p, accu.tail)
	      }
	    }
	  }
	filter0(p, this)
  }

  def union(that: TweetSet): TweetSet = {
    if (this.isEmpty) {that}
    else {
      if (that.isEmpty) {this}
      else {
        val h = this.head
        val t = this.tail
        if (that.contains(h)) {
          t.union(that)
        } else {
         that.incl(h)
         t.union(that)
        }
      }
    }
  }

  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending = ???

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /** This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
  // -------------------------------------------------------------------------
}

class Empty extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = ???

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
  // -------------------------------------------------------------------------
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = ???

  // The following methods are provided for you, and do not have to be changed
  // -------------------------------------------------------------------------
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  // -------------------------------------------------------------------------
}


/** This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /** Appends tw to the end of this sequence.
   */
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(4777); 
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus");System.out.println("""google  : List[java.lang.String] = """ + $show(google ));$skip(72); 
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad");System.out.println("""apple  : List[java.lang.String] = """ + $show(apple ));$skip(36); 

  val googleTweets: TweetSet = ???;System.out.println("""googleTweets  : objsets.TweetSet = """ + $show(googleTweets ));$skip(35); 

  val appleTweets: TweetSet = ???;System.out.println("""appleTweets  : objsets.TweetSet = """ + $show(appleTweets ));$skip(98); 

  // Q: from both sets, what is the tweet with highest #retweets?
  val trending: Trending = ???;System.out.println("""trending  : objsets.Trending = """ + $show(trending ))}
}

object Main extends App {
  // Some help printing the results:
  // println("RANKED:")
  // GoogleVsApple.trending foreach println
}