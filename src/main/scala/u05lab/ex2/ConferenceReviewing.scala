package u05lab.ex2

object ConferenceRev:

  enum Question:
    case Relevance, Significance, Confidence, Final

  trait ConferenceReviewing:
    def loadReview(art: Int, scores: Map[Question, Int]): Unit
    def loadReview(art: Int, rel: Int, sign: Int, conf: Int, fin: Int): Unit
    def orderedScores(art: Int, quest: Question): List[Int]
    def averageFinalScores(art: Int): Double
    def acceptedArticles(): Set[Int]
    def sortedAcceptedArticles(): List[(Int, Double)]
    def averageWeightedFinalScoreMap(): Map[Int, Double]

  object ConferenceReviewing:
    def apply(): ConferenceReviewing = ConferenceReviewingImpl()
    private class ConferenceReviewingImpl extends ConferenceReviewing:
      import ConferenceRev.Question.*

      var revs: List[(Int, Map[Question, Int])] = List()

      override def loadReview(art: Int, scores: Map[Question, Int]): Unit =
        revs = (art, scores) :: revs

      override def loadReview(art: Int, rel: Int, sign: Int, conf: Int, fin: Int): Unit =
        revs = (art, Map(Relevance -> rel, Significance -> sign, Confidence -> conf, Final -> fin)) :: revs

      override def orderedScores(art: Int, quest: Question): List[Int] =
        revs.collect({case (a, m) if a == art => m.get(quest).get})
            .sorted

      override def averageFinalScores(art: Int): Double =
        avg(revs.filter(_._1 == art), _._2.get(Final).get)

      private def accepted(art: Int): Boolean =
        averageFinalScores(art) > 5.0 &&
          revs.collect({case (a, m) if a == art => m.get(Relevance).get})
            .exists(_ >= 8)

      override def acceptedArticles(): Set[Int] =
        revs.map(_._1)
          .distinct
          .filter(accepted(_))
          .toSet

      override def sortedAcceptedArticles(): List[(Int, Double)] =
        acceptedArticles().toList
          .map(art => (art, averageFinalScores(art)))
          .sortBy((a, v) => v)

      private def averageWeightedFinalScore(art: Int): Double =
        avg(revs.collect({case (a, m) if a == art => m.get(Final).get * m.get(Confidence).get / 10.0}), v => v)

      override def averageWeightedFinalScoreMap(): Map[Int, Double] =
        revs.map(a => (a._1, averageWeightedFinalScore(a._1)))
          .toMap

      // instead of map + sum / length I found this method to calculate the avg
      private def avg[A](l: List[A], extractor: A => Double): Double =
        l.foldLeft((0.0, 1))((acc, elem) => (acc._1 + (extractor(elem) - acc._1) / acc._2, acc._2 + 1))._1


