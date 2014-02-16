package bpReduce
package ast

import scala.collection.mutable.ListBuffer
import bpReduce.writer.Formatter

final case class Program(globals: VariableHolder, functions: List[Function]) {

  override def toString: String = {
    Formatter.format(this)
  }

  /**
   * @param pf Applied to each expr on which the function is defined and collect the results.
   */
  def collect[T](pf: PartialFunction[Stmt, T]): List[T] = {
    val results = new ListBuffer[T]

    for {
      f <- functions
    } {
      results ++= f.collect(pf)
    }

    results.toList
  }

  def filter(predicate: Stmt => Boolean): List[Stmt] = {
    val results = new ListBuffer[Stmt]

    for {
      f <- functions
    } {
      results ++= f.filter(predicate)
    }

    results.toList
  }

  def exists(predicate: Stmt => Boolean): Boolean = {
    for {
      f <- functions
    } {
      if(f.contains(predicate)) return true
    }

    false
  }

}