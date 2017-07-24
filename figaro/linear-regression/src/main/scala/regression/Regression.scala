package regression



import coinflip.CoinFlip.{fairness, probability}
import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.language.{Apply, Chain, Flip}
import com.cra.figaro.library.atomic.continuous.{AtomicNormal, Beta, Normal, Uniform}

object Regression extends App {
  /**
    * Generate Data for Linear Regression, according to the model:
    * Y = alpha * x + beta (+ gaussian noise)
    * @param alpha coefficient of predictor x
    * @param beta  constant term
    * @param sigma standard deviation for gaussian noise (mean is 0)
    */
  def dataset(alpha: Double, beta: Double,
              sigma: Double, size: Int) : Seq[(Double, Double)] = {
    val x = Normal(0, 1)
    val noise = Normal(0, sigma)
    var xMean = 0.0
    for (_ <- 1 to size) yield {
      x.generate()
      noise.generate()
      (x.value, alpha * x.value + beta + noise.value)
    }
  }

  val data = dataset(2, 2, 1, 100)

  val alpha = Beta(0, 10)
  val beta = Beta(0, 10)

  val trials = for ((x, y) <- data) yield {
    val predictor = Normal(0, 1)
    val mu = Apply(alpha, beta, predictor, (a: Double, b: Double, x: Double) => a * x + b)

    predictor.observe(x)
    mu.observe(y)
    (predictor, mu)
  }

  var algorithm = EMWithBP(alpha)
  algorithm.start()
  algorithm.kill()

  algorithm = EMWithBP(beta)
  algorithm.start()
  algorithm.kill()

  println(s"Alpha: 2 learned: ${alpha.MAPValue}")
  println(s"Beta: 2 learned ${beta.MAPValue}")
}
