package u05lab.code

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
    override def getKind: Kind = kind

    override def getEvaluation: Option[Int] = kind match {
      case SUCCEDED() =>
        if (eval.get >= SUFFICIENT && eval.get <= PERFECT) eval
        else throw new IllegalArgumentException
      case _ => Option.empty
    }

    override def cumLaude: Boolean = eval.nonEmpty && laude
  }

  override def failed: Result = AbstractExamResult(FAILED())

  override def retired: Result = AbstractExamResult(RETIRED())

  override def succeded(evaluation: Int): Result = AbstractExamResult(SUCCEDED(), Option.apply(evaluation))

  override def succededCumLaude: Result = AbstractExamResult(SUCCEDED(), Option.apply(PERFECT), true)
}


//-----------EXAM MANAGER-----------
sealed trait ExamsManager {
  def createNewCall(call:String): Unit

  def addStudentResult(call:String, student:String, result:Kind)

  def getAllStudentsFromCall(call:String):Set[String]

  def getEvaluationsMapFromCall(call:String):Map[String, Int]

  def getResultsMapFromStudent(student:String):Map[String, String]

  def getBestResultFromStudent(student:String):Option[Int]

}

case class ExamsManagerImpl() extends ExamsManager {
  val exams: Map[String, Map[String, Result]] = Map.empty
  val check: Boolean => Unit = c => if(!c) throw new IllegalArgumentException

  override def createNewCall(call: String): Unit = ???

  override def addStudentResult(call: String, student: String, result: Kind): Unit = ???

  override def getAllStudentsFromCall(call: String): Set[String] = ???

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = ???

  override def getResultsMapFromStudent(student: String): Map[String, String] = ???

  override def getBestResultFromStudent(student: String): Option[Int] = ???
}


//-----------TEST-----------
object ExamsManagerTest extends App {

  /* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */

}