package models
import java.util.Date
import java.text.SimpleDateFormat

// example of a hook event
// HookEvent(date, "134", "a minor bug at XX", "can be fixed", Issues, IssueOpen)
case class HookEvent (eventDate: Date, number: String, title: String, body: String, eventType: HookEventType, eventState: HookEventState) {
  def dateString = new SimpleDateFormat("MMM d, yyyy").format(eventDate)
}

case object HookEvent {
  def commitHookEvent = {}
  def updateHookEvent = {}
}

sealed trait HookEventType {
  override def toString() = HookEventType.mapToString(this)

  def describe() =
    this match {
      case Hooks                => "an hook is setup" 
      case Issues               => "an issue"
      case IssueComment         => "an issue comment"
    }
}

object HookEventType {
  val mapToString: Map[HookEventType, String] = Map(
    Hooks                -> "hooks",
    Issues               -> "issues",
    IssueComment         -> "issue_comment"
  )
  def apply(s: String): HookEventType = mapToString.map(_.swap).apply(s)
}

case object Hooks                extends HookEventType
case object Issues               extends HookEventType // 
case object IssueComment         extends HookEventType // 


sealed trait HookEventState {
  override def toString() = HookEventState.mapToString(this)

  def describe() =
    this match {
      case NoState           => "nothing to describe"
      case IssueOpen         => "this issue is opened"
      case IssueTested       => "this issue has been tested"
      case IssueWaitReview   => "this issue is waiting for review"
      case IssueReviewed     => "this issue has been reviewed"
      case IssueClosed       => "this issue has been closed"
    }
}

object HookEventState {
  val mapToString: Map[HookEventState, String] = Map(
    IssueOpen         -> "issue_open",
    IssueTested       -> "issue_tested",
    IssueWaitReview   -> "issue_wait_review",
    IssueReviewed     -> "issue_reviewed",
    IssueClosed       -> "issue_closed"
  )
  def apply(s: String): HookEventState = mapToString.map(_.swap).apply(s)
}

case object NoState         extends HookEventState
case object IssueOpen       extends HookEventState 
case object IssueTested     extends HookEventState
case object IssueWaitReview extends HookEventState
case object IssueReviewed   extends HookEventState
case object IssueClosed     extends HookEventState 
