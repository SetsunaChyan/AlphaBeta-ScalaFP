package util

/**
 * Created by Setsuna on 2021/11/24 10:45
 */
object Cons {
    def apply[T](x: T, y: ListOf[T]): ListOf[T] = cons(x, y)
    def cons[T](x: T, y: ListOf[T]): ListOf[T] = new Cons(x, y)
    def take[T](f: Int => T, n: Int, now: Int = 0): ListOf[T] =
        if (n == now) Empty
        else Cons(f(now), take(f, n, now + 1))
}

trait ListOf[+T] {
    def head: T
    def tail: ListOf[T]
    def isEmpty: Boolean
    def printElements: String
    override def toString: String = "[" + printElements + "]"

    def reduce[A](f: (T, A) => A, zero: A): A =
        if (this.isEmpty) zero
        else f(head, tail.reduce(f, zero))

    def map[A](f: T => A): ListOf[A] =
        reduce(fromCurry(combine(toCurry(Cons.cons[A]), f)), Empty)
    // reduce((x: T, y: ListOf[A]) => Cons(f(x), y), Empty)
}

object Empty extends ListOf[Nothing] {
    override def head: Nothing = throw new NoSuchElementException("empty.head")
    override def tail: ListOf[Nothing] = throw new NoSuchElementException("empty.tail")
    override def isEmpty: Boolean = true
    override def printElements: String = ""
}

class Cons[T](h: T, t: ListOf[T]) extends ListOf[T] {
    override def head: T = h
    override def tail: ListOf[T] = t
    override def isEmpty: Boolean = false
    override def printElements: String =
        if (t.isEmpty) "" + h
        else h + " " + t.printElements
}
