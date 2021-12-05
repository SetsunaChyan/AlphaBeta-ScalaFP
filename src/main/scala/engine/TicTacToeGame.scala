package engine

import util.{Cons, Empty, ListOf, Pair, Stream}

/**
 * Created by Setsuna on 2021/11/25 12:54
 */
trait Position extends GamePos {
    def board: Array[Array[Int]]
    def isAITurn: Boolean
    def isPre: Boolean // 是否是先手 X先手 O后手
    def operation: List[Pair[Int, Int]]
    override def toString: String = boardToString
    def boardToString: String = {
        def chr(x: Int): Char =
            if (x == Position.BLANK) '.'
            else if (x == Position.CIRCLE) 'O'
            else if (x == Position.CROSS) 'X'
            else '?'

        def toString(id: Int, s: String): String = {
            val x = id / Position.len
            val y = id % Position.len
            val extra = if (y == Position.len - 1) "\n" else ""
            chr(board(x)(y)) + extra + s
        }

        reduce(toString, "")
    }

    def reduce[T](f: (Int, T) => T, zero: T, step: Int = 0): T =
        if (step == Position.len * Position.len) zero
        else f(step, reduce(f, zero, step + 1))

    def countBlank() = {
        def count(id: Int, sum: Int): Int =
            sum + {
                if (board(id / Position.len)(id % Position.len) == Position.BLANK) 1 else 0
            }

        reduce(count, 0)
    }

    def move(x: Int, y: Int, keep: Boolean = true): Position = Position.newPosition({
        val ch = if (isPre) Position.CROSS else Position.CIRCLE
        board.updated(x, board(x).updated(y, ch))
    }, !isAITurn, !isPre, {
        if (keep) operation.appended(new Pair(x, y))
        else Nil
    })

    def canMove(x: Int, y: Int): Boolean =
        x >= 0 && x < Position.len && y >= 0 && y < Position.len && board(x)(y) == Position.BLANK

    def isWin(): Boolean =
        Math.max(count(Position.CROSS), count(Position.CIRCLE)) == Position.len

    def count(tar: Int): Int = {
        def count(x: Int, y: Int, dx: Int, dy: Int, tar: Int, step: Int = 0): Int =
            if (step == Position.len) 0
            else {
                if (board(x)(y) == tar) 1 else 0
            } + count(x + dx, y + dy, dx, dy, tar, step + 1)

        val tmp: List[Int] = count(0, 0, 1, 0, tar) :: count(0, 1, 1, 0, tar) ::
            count(0, 2, 1, 0, tar) :: count(0, 0, 0, 1, tar) ::
            count(1, 0, 0, 1, tar) :: count(2, 0, 0, 1, tar) ::
            count(0, 0, 1, 1, tar) :: count(0, 2, 1, -1, tar) :: Nil
        tmp.reduce((x: Int, y: Int) => if (x > y) x else y)
    }
}

object Position {
    def len: Int = 3
    def fac: Int = 100
    val BLANK: Int = 0
    val CROSS: Int = 1
    val CIRCLE: Int = 2
    def InitPosition(p: Boolean): Position = new Position {
        override def board: Array[Array[Int]] = Array.ofDim[Int](Position.len, Position.len)
        override def isAITurn: Boolean = p
        override def isPre: Boolean = true // 是否是先手 X先手 O后手
        override def operation: List[Pair[Int, Int]] = Nil
    }
    def newPosition(b: Array[Array[Int]], p1: Boolean, p2: Boolean, l: List[Pair[Int, Int]]): Position = new Position {
        override def board: Array[Array[Int]] = b
        override def isAITurn: Boolean = p1
        override def isPre: Boolean = p2
        override def operation: List[Pair[Int, Int]] = l
    }
}

object TicTacToeGame extends Game[Position] {
    def InitPos(p: Boolean): Position = Position.InitPosition(p)

    def bestMove(p: Position): Position = {
        // println(evaluate(p).v)
        val pa = evaluate(p).data.asInstanceOf[Position].operation.head
        p.move(pa.first, pa.second, false)
    }

    override def moves(p: Position): Stream[Position] = {
        def gen(id: Int, stream: Stream[Position]): Stream[Position] = {
            val x = id / Position.len
            val y = id % Position.len
            if (!p.canMove(x, y)) stream
            else Stream(p.move(x, y), stream)
        }

        if (p.isWin) Stream.empty
        else p.reduce(gen, Stream.empty)
    }


    override def static(p: Position): Choice = {
        val cx = p.count(Position.CROSS)
        val co = p.count(Position.CIRCLE)
        val v = if (p.isAITurn != p.isPre) -Position.fac else Position.fac
        new Choice({
            if (cx == Position.len) v * Position.len - p.operation.length
            else if (co == Position.len) -v * Position.len + p.operation.length
            else v * (cx - co)
        }, p)
    }
}
