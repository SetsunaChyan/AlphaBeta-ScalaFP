package util

/**
 * Created by Setsuna on 2021/11/24 14:03
 */
object combine {
    def apply[A, B, C](g: B => C, f: A => B): A => C =
        (x: A) => g(f(x))
}

object toCurry {
    def apply[A, B, C](f: (A, B) => C): A => B => C =
        (x: A) => (y: B) => f(x, y)
}

object fromCurry {
    def apply[A, B, C](f: A => B => C): (A, B) => C =
        (x: A, y: B) => f(x)(y)
}

class Pair[A, B](x: A, y: B) {
    def first: A = x
    def second: B = y
}