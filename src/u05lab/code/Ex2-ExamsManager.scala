package u05lab.code

import scala.collection.IterableOnce
import scala.jdk.CollectionConverters._

//-----------EXAM RESULTS-----------
sealed trait Kind

case class RETIRED() extends Kind
case class FAILED() extends Kind
case class SUCCEDED() extends Kind

object Kind {
  RETIRED
  FAILED
  SUCCEDED
}

sealed trait Result {
  def getKind:Kind
  def getEvaluation: Option[Int]
  def cumLaude: Boolean
}


//-----------EXAM RESULTS FACTORY-----------
sealed trait ExamResultFactory {
  def failed:Result
  def retired:Result
  def succeded(evaluation:Int):Result
  def succededCumLaude:Result
}

object ExamResult extends ExamResultFactory {

  val SUFFICIENT = 18
  val PERFECT = 30

  private case class AbstractExamResult(kind:Kind, eval:Option[Int] = Option.empty, laude:Boolean = false) extends Result {

    override def getKind: Kind =
      kind

    override def getEvaluation: Option[Int] = kind match {
      case SUCCEDED() => eval
      case _ => Option.empty
    }

    override def cumLaude: Boolean =
      eval.nonEmpty && laude
  }

  override def failed: Result =
    AbstractExamResult(FAILED())

  override def retired: Result =
    AbstractExamResult(RETIRED())

  override def succeded(evaluation: Int): Result =
      if (evaluation < SUFFICIENT || evaluation > PERFECT)
        throw new IllegalArgumentException
      else AbstractExamResult(SUCCEDED(), Option.apply(evaluation))

  override def succededCumLaude: Result =
    AbstractExamResult(SUCCEDED(), Option.apply(PERFECT), true)
}


//-----------EXAM MANAGER-----------
sealed trait ExamsManager {
  def createNewCall(call:String): Unit
  def addStudentResult(call:String, student:String, result:Result):Unit
  def getAllStudentsFromCall(call:String):Set[String]
  def getEvaluationsMapFromCall(call:String):Map[String, Int]
  def getResultsMapFromStudent(student:String):Map[String, String]
  def getBestResultFromStudent(student:String):Option[Int]

}

case class ExamsManagerImpl() extends ExamsManager {
  val check: Boolean => Unit = c => if(!c) throw new IllegalArgumentException

  var exams: Map[String, Map[String, Result]] = Map.empty

  override def createNewCall(call: String): Unit = {
    check(!exams.contains(call))
    exams = exams + (call -> Map.empty)
  }

  override def addStudentResult(call: String, student: String, result: Result): Unit = {
    check(exams.contains(call))
    check(!exams(call).contains(student))
    var exam = exams(call)
    exam = exam + (student -> result)
    exams = exams + (call -> exam)
  }

  override def getAllStudentsFromCall(call: String): Set[String] = {
    check(exams.contains(call))
    exams(call).keySet
  }

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
    check(exams.contains(call))
    exams(call).collect({ case (s, r) if r.getKind == SUCCEDED() => s -> r.getEvaluation.get })
  }

  override def getResultsMapFromStudent(student: String): Map[String, String] = {
    exams.collect({case (exam, evals) if evals.contains(student) => exam -> exams(exam)(student).getKind.toString })
  }

  override def getBestResultFromStudent(student: String): Option[Int] = {
    exams.collectFirst({case (_, evals) if evals.contains(student) => evals.collect({case (s,r) if s == student && r.getKind == SUCCEDED() => r.getEvaluation.get}).max((x:Int, y:Int)=>x-y) })
  }
}