package controllers

import akka.util.duration._
import models.Setting

object Config {
  val commitsPerPage = 50
  
  lazy val enablePollings = Setting.setting("enablePollings") == "true"
  
  val updatePollFrequency = 1 minute
  val refreshRunningBuildsFrequency = 5 minutes
  
  val githubUser = "scala"
  val githubRepo = "scala"
  val githubBranch = "master"

  val jenkinsUrl = "https://scala-webapps.epfl.ch/jenkins/"
  val jenkinsJob = "test"
    
  val hookUrl = "http://scalahookevents.herokuapp.com/"

//  val localGitRepoDir = "git-repo"

  //val revListerUrl = "http://scala-webapps.epfl.ch/rev-lister/"
  val revListerUrl = "http://localhost:9000/rev-lister/"

//  val artifactsDir = "/Users/luc/Downloads/backup/artifacts"

  lazy val jenkinsUsername = Setting.setting("jenkinsUsername")
  lazy val jenkinsPassword = Setting.setting("jenkinsPassword")
  
  val newCommitBuildRecipients = "scala-reports@epfl.ch"
  val manualBuildRecipients    = "lukas.rytz@epfl.ch"
  
  val oldestImportedCommit = "0cffdf38d9e2d88e66d8649d317f8815716b2748"
}
