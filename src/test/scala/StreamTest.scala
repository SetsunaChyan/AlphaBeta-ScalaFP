import util.{Cons, Stream}

/**
 * Created by Setsuna on 2021/11/25 9:31
 */
object StreamTest extends App {
    def next(N: Double)(x: Double): Double = (x + N / x) / 2;

    def repeat[A](f: A => A, a0: A): Stream[A] =
        Stream(a0, repeat(f, f(a0)))

    //@scala.annotation.tailrec
    def within(eps: Double, l: => Stream[Double]): Double =
        if (Math.abs(l.head - l.tail.head) < eps) l.tail.head
        else within(eps, l.tail)

    def sqrt(N: Double, eps: Double = 1) =
        within(eps, repeat(next(N), N))

    println(sqrt(2, 1e-8)) // 1.414213562373095

    val infiniteSeq = repeat(next(2.0),2.0)
    println(infiniteSeq.map(2*_).take(5))
}
