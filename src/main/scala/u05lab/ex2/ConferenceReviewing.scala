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

  class ConferenceReviewingImpl extends ConferenceReviewing:
    import ConferenceRev.Question.*

    var revs: List[(Int, Map[Question, Int])] = List()

    def loadReview(art: Int, scores: Map[Question, Int]): Unit =
      revs = (art, scores) :: revs

    def loadReview(art: Int, rel: Int, sign: Int, conf: Int, fin: Int): Unit =
      revs = (art, Map(Relevance -> rel, Significance -> sign, Confidence -> conf, Final -> fin)) :: revs

    def orderedScores(art: Int, quest: Question): List[Int] =
      revs.filter(_._1 == art)
        .map(_._2.get(quest).get)
        .sorted

    def averageFinalScores(art: Int): Double =
      // instead of map + sum / lenght I found this method on the Internet
      revs.filter(_._1 == art)
        .foldLeft((0.0, 1))((acc, elem) => (acc._1 + (elem._2.get(Final).get - acc._1) / acc._2, acc._2 + 1))._1

    def acceptedArticles(): Set[Int] = ???

    def sortedAcceptedArticles(): List[(Int, Double)] = ???

    def averageWeightedFinalScoreMap(): Map[Int, Double] = ???

