package fpinscala.monoids.isomorphism

import fpinscala.monoids.Monoid

// A Stanoid (named after me) is just a monoid together with its elements
// it is the simplest representation I could think of for a finite Monoid
case class Stanoid[A](monoid: Monoid[A], elems: List[A]) {
  def computeCayleyTable: CayleyTable[A] = CayleyTable {
    val allCombinations = for { x <- elems; y <- elems } yield monoid.op(x,y)
    allCombinations.grouped(elems.length).toList
  }
}

object Stanoid {
  def areIsomorphic[A,B](x: Stanoid[A], y: Stanoid[B]): Boolean =
    CayleyTable.areIsomorphic(x.computeCayleyTable, y.computeCayleyTable)
}

// this is a simple way of representing the structure of a finite monoid
// https://en.wikipedia.org/wiki/Cayley_table
case class CayleyTable[A](table: List[List[A]]) {
  import CayleyTable._

  def show: Unit = {
    println("Cayley Table:")
    table.foreach(row => {
      println(row.map(_.toString).reduce((x,y) => s"${x}, ${y}"))
    })
  }

  def toSortedNumericTable: CayleyTable[Int] = {
    val rep = table.flatten.distinct.zipWithIndex.toMap[A, Int]
    CayleyTable {
      val intTable = deepMap(rep)(table)
      intTable.sortBy(_.head)
    }
  }
}

object CayleyTable {
  def deepMap[A,B](f: A => B)(xss: List[List[A]]): List[List[B]] =
    xss.map(_.map(f))

  // need to compare one Cayley table with all permutations of the other
  // all we're trying to establish is if they have the same structure, not the same contents
  def areIsomorphic[A,B](as: CayleyTable[A],  bs: CayleyTable[B]): Boolean = {
    val List(aNums, bNums) = List(as, bs).map(_.toSortedNumericTable)
    as.toSortedNumericTable == bs.toSortedNumericTable
  }

  def permuteCayleyTable(numericTable: CayleyTable[Int]): CayleyTable[Int] = {
    val table = numericTable.table
    val mod = table.length

    def nextInt(x: Int) = if (x == mod - 1) 0 else x + 1

    CayleyTable { deepMap(nextInt)(table) }
  }

  // this is obscenely dangerous because if you call it with something that never returns to its initial state you die
  @annotation.tailrec
  def obtainCycle[A](init: A, f: A => A, res: List[A]): List[A] = res match {
    case Nil => obtainCycle(init, f, List(init))
    case h :: Nil => obtainCycle(init, f, f(h) :: (h :: Nil))
    case h :: _ => {
      val next = f(h)
      if (next == init) res else obtainCycle(init, f, next :: res)
    }
  }

  def getAllPermutations(numericTable: CayleyTable[Int]): List[CayleyTable[Int]] =
    obtainCycle(numericTable, permuteCayleyTable, Nil)
}

object CheckIsomorphisms extends App {
  import fpinscala.monoids.OrderTwoMonoids.{nonGroupMonoid, E, A}
  val nonGroupCayley = Stanoid(nonGroupMonoid, List(E(), A())).computeCayleyTable.toSortedNumericTable
  val nonGroupCayley1 = Stanoid(nonGroupMonoid, List(A(), E())).computeCayleyTable.toSortedNumericTable

  val booleanCayley = Stanoid(Monoid.booleanOr, List(true, false)).computeCayleyTable.toSortedNumericTable
  val booleanCayley1 = Stanoid(Monoid.booleanOr, List(false, true)).computeCayleyTable.toSortedNumericTable

  CayleyTable.getAllPermutations(nonGroupCayley).foreach(_.show)
//  println(Stanoid.areIsomorphic(booleanStanoid1, booleanStanoid1))
//  println(Stanoid.areIsomorphic(booleanStanoid, booleanStanoid1))
}


