package controllers

import akka.util.duration.intToDurationInt

object Config {
  
  val gitHubUser = "taolee"
  val gitHubPassword = "taolee123"
  val gitHubRepo = "scalahooks"
  val hookUrl = "http://scalahooks.herokuapp.com/githubMsg"
  val gitHubUrl = "https://api.github.com/repos/" + gitHubUser + "/" + gitHubRepo
  //val hookUrl = "http://requestb.in/1imhe4b1"
 
  val reviewerList = List("taolee", "adriaan___", "odersky___", "lrytz___", "heather___", "vlad___")
  val reviewMsgList = List("Review", "review", "REVIEW")
  val reviewedMsgList = List("LGTM", "lgtm")
  val defaultLabelList = List("tested", "reviewed")
  //val buildBot = "scala-jenkins"
  val buildBot = "taolee"

  val waitMinInterval = 12 * 48 // 48 hours = (12 * 5) * 48 minutes
  val waitHourInterval = 6
  val waitDayInterval = 2
  val updateFrequency = 10 minutes
  val initialDelay = 1 minutes
  val enableActor = true
  
  val pullRequestMilestoneMap = Map("taolee:master" -> "RC1", 
                                    "taolee:heroku" -> "RC0")
}