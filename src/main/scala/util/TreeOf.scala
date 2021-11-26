package util

/**
 * Created by Setsuna on 2021/11/24 12:35
 */
object Node {
    def apply[T](x: T, sons: => Stream[TreeOf[T]]): TreeOf[T] = node(x, sons)
    def node[T](x: T, sons: => Stream[TreeOf[T]]): TreeOf[T] = new Node(x, sons)
}

trait TreeOf[+T] {
    def label: T
    def sons: Stream[TreeOf[T]]
    override def toString: String =
        label + {
            if (!sons.isEmpty) " " + sons.toString() else ""
        }

    def reduce[A, B](f: (T, => B) => A, g: (A, => B) => B, zero: B): A = {
        def reduceStream(l: Stream[TreeOf[T]], f: (T, => B) => A, g: (A, => B) => B, zero: B): B =
            if (l.isEmpty) zero
            else g(l.head.reduce(f, g, zero), reduceStream(l.tail, f, g, zero))

        f(label, reduceStream(sons, f, g, zero))
    }

    def map[A](f: T => A): TreeOf[A] = {
        def tmp(hd: T, sons: => Stream[TreeOf[A]]): TreeOf[A] = Node.node[A](f(hd), sons)

        reduce(tmp, Stream.cons[TreeOf[A]], Stream.empty)
    }
}

class Node[T](x: T, s: Stream[TreeOf[T]]) extends TreeOf[T] {
    override def label: T = x
    override def sons: Stream[TreeOf[T]] = s
}

