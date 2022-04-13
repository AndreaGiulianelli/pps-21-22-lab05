package u05lab.ex2

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Before
import u05lab.ex2.ConferenceRev.*

class ConferenceReviewingTest:
  val cr = ConferenceReviewing()
  cr.loadReview(1, 8, 8, 6, 8); // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
  // e simile per gli altri
  cr.loadReview(1, 9, 9, 6, 9); // 5.4
  cr.loadReview(2, 9, 9, 10, 9); // 9.0
  cr.loadReview(2, 4, 6, 10, 6); // 6.0
  cr.loadReview(3, 3, 3, 3, 3); // 0.9
  cr.loadReview(3, 4, 4, 4, 4); // 1.6
  cr.loadReview(4, 6, 6, 6, 6); // 3.6
  cr.loadReview(4, 7, 7, 8, 7); // 5.6
  cr.loadReview(4,Map(Question.Relevance -> 8, Question.Significance -> 8, Question.Confidence -> 7, Question.Final -> 8));
  cr.loadReview(5, 6, 6, 6, 10); // 6.0
  cr.loadReview(5, 7, 7, 7, 10); // 7.0

  @Test
  def testOrderedScores(): Unit =
    assertEquals(List(4, 9), cr.orderedScores(2, Question.Relevance))

  @Test
  def testAverageFinalScore(): Unit =
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)

  @Test
  def testAcceptedArticles(): Unit =
    assertEquals(Set(1, 2, 4), cr.acceptedArticles())

  @Test
  def testSortedAcceptedArticles(): Unit =
    assertEquals(List((4, 7.0), (2, 7.5), (1, 8.5)), cr.sortedAcceptedArticles())

  @Test
  def testOptionalTestAverageWeightedFinalScore(): Unit =
    assertEquals((4.8 + 5.4) / 2, cr.averageWeightedFinalScoreMap().get(1).get, 0.01)