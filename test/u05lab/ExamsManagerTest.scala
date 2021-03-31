package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows}
import org.junit.jupiter.api.Test
import u05lab.code.{ExamResult, ExamsManager, ExamsManagerImpl, FAILED, RETIRED, Result, SUCCEDED}

class ExamsManagerTest {

  val TOO_BAD = 17
  val BAD = 18
  val GOOD = 28
  val PERFECT = 30
  val TOO_MUCH = 31

  val em:ExamsManager = new ExamsManagerImpl

  val jan = "gennaio"
  val feb = "febbraio"
  val mar = "marzo"

  val st1 = "rossi"
  val st2 = "bianchi"
  val st3 = "verdi"
  val st4 = "neri"
  val st5 = "viola"

  @Test
  def testExamResultBasicBehaviour: Unit = {
    val f:Result = ExamResult.failed
    assertEquals(f.getKind, FAILED())
    assertEquals(f.getEvaluation, Option.empty)
    assertEquals(f.cumLaude, false)

    val r:Result = ExamResult.retired
    assertEquals(r.getKind, RETIRED())
    assertEquals(r.getEvaluation, Option.empty)
    assertEquals(r.cumLaude, false)

    val s:Result = ExamResult.succeded(GOOD)
    assertEquals(s.getKind, SUCCEDED())
    assertEquals(s.getEvaluation, Option.apply(GOOD))
    assertEquals(s.cumLaude, false)

    val l:Result = ExamResult.succededCumLaude
    assertEquals(l.getKind, SUCCEDED())
    assertEquals(l.getEvaluation, Option.apply(PERFECT))
    assertEquals(l.cumLaude, true)
  }

  @Test
  def testEvaluationBounds(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => ExamResult.succeded(TOO_BAD))
    assertThrows(classOf[IllegalArgumentException], () => ExamResult.succeded(TOO_MUCH))
  }

  private def prepareExams(): Unit = {
    em.createNewCall(jan)
    em.createNewCall(feb)
    em.createNewCall(mar)

    em.addStudentResult(jan, st1, ExamResult.failed)
    em.addStudentResult(jan, st2, ExamResult.retired)
    em.addStudentResult(jan, st3, ExamResult.succeded(GOOD))
    em.addStudentResult(jan, st4, ExamResult.succededCumLaude)

    em.addStudentResult(feb, st1, ExamResult.failed)
    em.addStudentResult(feb, st2, ExamResult.retired)
    em.addStudentResult(feb, st3, ExamResult.succeded(PERFECT))
    em.addStudentResult(feb, st4, ExamResult.succededCumLaude)

    em.addStudentResult(mar, st3, ExamResult.succeded(GOOD))
    em.addStudentResult(mar, st4, ExamResult.retired)
    em.addStudentResult(mar, st5, ExamResult.failed)
  }

  @Test
  def testExamsManagement(): Unit = {
    prepareExams()

    //partecipanti agli appelli
    assertEquals(em.getAllStudentsFromCall(jan), Set(st1, st2, st3, st4))
    assertEquals(em.getAllStudentsFromCall(mar), Set(st3, st4, st5))

    //promossi di gennaio
    assertEquals(em.getEvaluationsMapFromCall(jan).size, 2)
    assertEquals(em.getEvaluationsMapFromCall(jan)(st3), GOOD)
    assertEquals(em.getEvaluationsMapFromCall(jan)(st4), PERFECT)

    //promossi di febbraio
    assertEquals(em.getEvaluationsMapFromCall(feb).size, 2)
    assertEquals(em.getEvaluationsMapFromCall(feb)(st3), PERFECT)
    assertEquals(em.getEvaluationsMapFromCall(feb)(st4), PERFECT)

    //tutti i risultati di rossi
    assertEquals(em.getResultsMapFromStudent(st1).size, 2)
    assertEquals(em.getResultsMapFromStudent(st1)(jan), "FAILED()")
    assertEquals(em.getResultsMapFromStudent(st1)(feb), "FAILED()")

    //tutti i risultati di verdi
    assertEquals(em.getResultsMapFromStudent(st3).size, 3)
    assertEquals(em.getResultsMapFromStudent(st3)(jan), "SUCCEDED()")
    assertEquals(em.getResultsMapFromStudent(st3)(feb), "SUCCEDED()")
    assertEquals(em.getResultsMapFromStudent(st3)(mar), "SUCCEDED()")

  }

  @Test
  def testBestResult(): Unit = {
    this.prepareExams()
    assertEquals(em.getBestResultFromStudent(st1),Option.empty)
    assertEquals(em.getBestResultFromStudent(st2),Option.empty)
    assertEquals(em.getBestResultFromStudent(st3),Option.apply(PERFECT))
    assertEquals(em.getBestResultFromStudent(st4),Option.apply(PERFECT))
    assertEquals(em.getBestResultFromStudent(st5),Option.empty)
  }

}
