package coinflip


import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.continuous.Beta

object CoinFlip extends App {
  def toss(prob: Double, n: Int): Seq[Boolean] = {
    val coin = Flip(prob)
    for (_ <- 1 to n) yield {
      coin.generate()
      coin.value
    }
  }

  // First get a lot of tosses (learning data).
  val probability = 0.5
  val tosses = toss(probability, 1000)

  // Parameter modeling the bias in our coin.  (Apparently the conjugate prior
  // for a Bernoulli: Flip is a Beta distribution.)
  val fairness = Beta(1, 1)

  // Construct an element for each trial; observe() with the value from tosses.
  val trials = for (t <- tosses) yield {
    val trial = Flip(fairness)
    trial.observe(t)
    trial
  }

  // Instantiate the learning algorithm, and run it.
  val algorithm = EMWithBP(fairness)
  algorithm.start()
  algorithm.kill()

  println(s"Actual: $probability learned: ${fairness.MAPValue}")
}
