package engine

import util.{Node, Stream, TreeOf}

/**
 * Created by Setsuna on 2021/11/24 10:45
 */
trait GamePos {
}

class Choice(x: Int, y: Any) extends Ordered[Choice] {
    def v: Int = x
    def data: Any = y
    override def compare(that: Choice): Int = v - that.v
    override def toString: String = v +"\n"+ data.toString
}

object Game {
    def repTree[Pos <: GamePos](f: Pos => Stream[Pos])(a: Pos): TreeOf[Pos] =
        Node(a, f(a).map(repTree(f)))

    def lazyMax(x: Choice, y: => Choice): Choice = if (x > y) x else y
    def lazyMin(x: Choice, y: => Choice): Choice = if (x < y) x else y
    /*
    def maximise(t: TreeOf[Choice]): Choice =
        if (t.sons.isEmpty) t.label
        else t.sons.map(minimise).reduce(lazyMax, t.sons.head.label)
    def minimise(t: TreeOf[Choice]): Choice =
        if (t.sons.isEmpty) t.label
        else t.sons.map(maximise).reduce(lazyMin, t.sons.head.label)
    */
    def maximise(t: TreeOf[Choice]): Choice = {
        val tmp=maximise_(t)
        tmp.reduce(lazyMax, tmp.head)
    }
    def maximise_(t: TreeOf[Choice]): Stream[Choice] =
        if (t.sons.isEmpty) Stream(t.label, Stream.empty)
        else mapMin(t.sons.map(minimise_))
    def minimise_(t: TreeOf[Choice]): Stream[Choice] =
        if (t.sons.isEmpty) Stream(t.label, Stream.empty)
        else mapMax(t.sons.map(maximise_))
    def mapMin(l: Stream[Stream[Choice]]): Stream[Choice] = {
        val pot = l.head.reduce(lazyMin, l.head.head)
        Stream(pot, omit_min(pot, l.tail))
    }
    def mapMax(l: Stream[Stream[Choice]]): Stream[Choice] = {
        val pot = l.head.reduce(lazyMax, l.head.head)
        Stream(pot, omit_max(pot, l.tail))
    }
    def omit_min(pot: Choice, l: Stream[Stream[Choice]]): Stream[Choice] =
        if (l.isEmpty) Stream.empty
        else if (minLeq(l.head, pot)) omit_min(pot, l.tail)
        else {
            val pot = l.head.reduce(lazyMin, l.head.head)
            Stream(pot, omit_min(pot, l.tail))
        }
    def omit_max(pot: Choice, l: Stream[Stream[Choice]]): Stream[Choice] =
        if (l.isEmpty) Stream.empty
        else if (maxGeq(l.head, pot)) omit_max(pot, l.tail)
        else {
            val pot = l.head.reduce(lazyMax, l.head.head)
            Stream(pot, omit_max(pot, l.tail))
        }
    @scala.annotation.tailrec
    def minLeq(l: Stream[Choice], pot: Choice): Boolean =
        if (l.isEmpty) false
        else if (l.head <= pot) true
        else minLeq(l.tail, pot)
    @scala.annotation.tailrec
    def maxGeq(l: Stream[Choice], pot: Choice): Boolean =
        if (l.isEmpty) false
        else if (l.head >= pot) true
        else maxGeq(l.tail, pot)

    def prune[Pos <: GamePos](dep: Int)(t: TreeOf[Pos]): TreeOf[Pos] =
        if (dep == 0) Node(t.label, Stream.empty)
        else Node(t.label, t.sons.map(prune[Pos](dep - 1)))
}

abstract class Game[Pos <: GamePos] {
    def gameTree(p: Pos): TreeOf[Pos] = Game.repTree(moves)(p)
    def moves(p: Pos): Stream[Pos]
    def static(p: Pos): Choice
    def MaxDep: Int = 8
    def evaluate(p: Pos): Choice =
        Game.maximise(Game.prune(MaxDep)(gameTree(p)).map(static))
}
