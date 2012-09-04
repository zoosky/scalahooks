package models
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.util.Date

case class MalFormedJSONPayloadException(msg: String) extends RuntimeException {
  override def toString: String = msg
}

case class MissingDefaultLabelsException(msg: String) extends RuntimeException {
  override def toString: String = msg
}

case class Comment (action: CommentAction, id: Long, body: String, createTime: Date, updateTime: Date, userLogin: String) {
  
  def Id: Long = {id}
  def Body: String = {body}
  def CreateTime: String = {createTime.toString()}
  def getCreateTime: Long = {createTime.getTime()}
  def UpdateTime: String = {updateTime.toString()}
  def getUpdateTime: Long = {updateTime.getTime()}
  def User: String = {userLogin}
  def Action = {action}
}

class Review(reviewer: String, var rStatus: ReviewStatus) {
  def getReviewer = reviewer.drop(1)
  def getRStatus = rStatus
  def setRStatus(status: ReviewStatus) = {rStatus = status}
}

class Issue (number: Long, var title: String, var body: String) {
  private var rStatus: ReviewStatus = ReviewNone
  private var bState: BuildState = BuildNone
  private var tState: TestState = TestNone
  var labels = new ListBuffer[String]()
  var commentList = new ListBuffer[Comment]()
  var reviewList = new ListBuffer[Review]()
  def Number: Long = {number}
  def Title: String = {title}
  def Body: String = {body}
  def getBState: BuildState = {bState}
  def updateBState(state: BuildState) = {bState = state}
  def getTState: TestState = {tState}
  def updateTState(state: TestState) = {tState = state}
  def getRStatus: ReviewStatus = {rStatus}
  def updateRStatus(status: ReviewStatus) = {rStatus = status}
  def updateTitle(title: String) = {this.title = title}
  def updateBody(body: String) = {this.body = body}
  override def toString(): String = {
    val issueString = "Issue number: " + this.number.toString() + "\n" +
                      "Issue title: "  + this.title             + "\n" +
                      "Issue body: "   + this.body              + "\n"
    var commentString = ""
    commentList.map(comment => commentString += comment.Body + "\n")
    if (commentString != "") commentString = "Comment body: \n" + commentString
    issueString + commentString
  }
  def describe: String = {
    "<< " + title + " >> " + body
  }
  def isTested: Boolean = {
    return labels.contains("tested")
  }
  def isReviewed: Boolean = {
    return labels.contains("reviewed")
  }
}

sealed trait BuildState {
  override def toString() = BuildState.mapToString(this)
}

object BuildState {
  val mapToString: Map[BuildState, String] = Map(
    BuildNone              -> "build none",
    BuildStart             -> "build started",
    BuildSuccess           -> "build successful",
    BuildFailure           -> "build failed"
  )

  def apply(s: String): BuildState = mapToString.map(_.swap).apply(s)
}

case object BuildNone             extends BuildState
case object BuildStart            extends BuildState 
case object BuildSuccess          extends BuildState
case object BuildFailure          extends BuildState

sealed trait TestState {
  override def toString() = TestState.mapToString(this)
}

object TestState {
  val mapToString: Map[TestState, String] = Map(
    TestNone               -> "test none",
    TestStart              -> "test started",
    TestSuccess            -> "test successful",
    TestFailure            -> "test failed"
  )

  def apply(s: String): TestState = mapToString.map(_.swap).apply(s)
}

case object TestNone              extends TestState
case object TestStart             extends TestState
case object TestSuccess           extends TestState
case object TestFailure           extends TestState

sealed trait ReviewStatus {}
object ReviewStatus {}

case object ReviewNone            extends ReviewStatus
case object ReviewOpen            extends ReviewStatus
case object ReviewFault           extends ReviewStatus
case object ReviewDone            extends ReviewStatus
case object ReviewWarning         extends ReviewStatus

sealed trait CommentAction {}
object CommentAction {}

case object CommentCreated        extends CommentAction
case object CommentDeleted        extends CommentAction

sealed trait CommentType {}
object CommentType {}

case object UnknownComment             extends CommentType           
case object BuildComment          extends CommentType
case object TestComment           extends CommentType
case object ReviewComment         extends CommentType


