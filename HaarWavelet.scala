/**
  * Author vipul vaibhaw on 1/11/2017.
  * Inspired by https://gist.github.com/boukeversteegh/0852313953eac565b708
  */

import scala.collection.mutable.ListBuffer

object HaarWavelet {

  def main(args: Array[String]): Unit = {
    val samples = ListBuffer(
      ListBuffer(1,4),
      ListBuffer(6,1),
      ListBuffer(0,2,4,6,7,7,7,7),
      ListBuffer(1,2,3,4),
      ListBuffer(7,5,1,6,3,0,2,4),
      ListBuffer(0),
      ListBuffer(3,2,3,7,5,5,1,1,0,2,5,1,2,0,1,2,0,2,1,0,0,2,1,2,0,2,1,0,0,2,1,2),
      ListBuffer(1,2,3,4,5,6,7,8,9)
    )

    for (i <- samples.indices){
      if(is_pow2(samples(i).length) & samples(i).length >= 2){
        println( "Input:        " + samples(i).mkString("[" ," ," ,"]"))
        val ubound = samples(i).max+1
        val length = samples(i).length
        val deltas = encode(samples(i), ubound)
        val avg = deltas._2

        println( "Input:        boundary = %s, length = %s" format (ubound, length))
        println( "Haar output:  %s, average = %s" format(deltas._1.mkString("[" ," ," ,"]"), avg))
        println( "Decoded:      " +decode(deltas._1, avg, ubound))
        println("\n")
      }
      else
        println("List length should be a power of 2, and larger than 2\n")
    }
  }

  def wrap(value:Int, ubound:Int):Int = {
    (value+ubound)%ubound
  }

  def encode(lst1:ListBuffer[Int], ubound:Int):(ListBuffer[Int],Int)={
    var lst = lst1
    var deltas = new ListBuffer[Int]()
    var avg = 0

    while (lst.length>=2) {
      var avgs = new ListBuffer[Int]()

      while (lst.nonEmpty) {
        // getting first two element from the list and removing them
        val a = lst.head
        lst.remove(0) // removing index 0 element from the list
        val b = lst.head
        lst.remove(0) // removing index 0 element from the list

        if (a<=b) {
          avg = (a + b)/2
        }
        else{
          avg = (a+b+ubound)/2
        }
        var delta = wrap(b-a,ubound)
        avgs += avg
        deltas += delta
      }
      lst = avgs
    }
    (deltas, avg%ubound)
  }

  def decode(deltas:ListBuffer[Int],avg:Int,ubound:Int):String={
    var avgs = new ListBuffer[Int]
    avgs += avg
    var l:Int = 1
    while(deltas.nonEmpty){
      var i = 0
      while(i < l ){
        val delta = deltas.last
        deltas.remove(deltas.length-1)
        val avg = avgs.last
        avgs.remove(avgs.length-1)

        val a = wrap(math.ceil(avg-delta/2.0).toInt,ubound)
        val b = wrap(math.ceil(avg+delta/2.0).toInt,ubound)

        //Prepending elements to ListBuffer
        avgs = avgs.reverse
        avgs += b
        avgs += a
        avgs = avgs.reverse

        i=i+1

      }
      l=l*2
    }
    avgs.mkString("[", ", ", "]")
  }

  def is_pow2(n:Int):Boolean={
    (n & -n) == n
  }
}
