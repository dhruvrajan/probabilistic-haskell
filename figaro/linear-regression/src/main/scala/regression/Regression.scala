package regression



import com.cra.figaro.algorithm.learning.EMWithBP
import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.continuous.{AtomicNormal, Beta, Normal, Uniform}

object Regression extends App {
  /**
    * Generate Data for Linear Regression, according to the model:
    * Y = alpha * x + beta (+ gaussian noise)
    * @param alpha coefficient of predictor x
    * @param beta  constant term
    * @param sigma standard deviation for gaussian noise (mean is 0)
    */
  def dataset(alpha: Double, beta: Double, sigma: Double, size: Int): Seq[(Double, Double)] = {
    val x = Normal(0, 1)
    val noise = Normal(0, sigma)
    for (_ <- 1 to size) yield {
      x.generate()
      noise.generate()
      (x.value, alpha * x.value + beta + noise.value)
    }
  }

  val alpha = 0.5
  val beta = 1
}
