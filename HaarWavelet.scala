/**
  * Created by vipul vaibhaw on 1/11/2017.
  */

import scala.collection.mutable.{ListBuffer, MutableList,ArrayBuffer}

object HaarWavelet {

  def main(args: Array[String]): Unit = {
    var samples = ListBuffer(
      ListBuffer(1,4),
      ListBuffer(6,1),
      ListBuffer(0,2,4,6,7,7,7,7),
      ListBuffer(1,2,3,4),
      ListBuffer(7,5,1,6,3,0,2,4),
    ListBuffer(3,2,3,7,5,5,1,1,0,2,5,1,2,0,1,2,0,2,1,0,0,2,1,2,0,2,1,0,0,2,1,2)
    )

    //println(samples)
    //println(samples(0))
    //println(samples.length)
    for (i <- 0 to samples.length-1){
      val ubound = samples(i).max+1
      //println(ubound)
      val length = samples(i).length
      //println(length)
      val deltas1 = encode(samples(i), ubound)
      //println(deltas1)
      val deltas = deltas1._1
      println(deltas)
      val avg = deltas1._2


      println( "Input:      %s, boundary = %s, length = %s" format(samples(i), ubound, length))
      println( "Haar output:%s, average = %s" format(deltas, avg))
      //println("Decoded:    %s" format(decode(deltas, avg, ubound)))
      println("\n")
    }
  }

  def wrap(value:Int, ubound:Int):Int = {
    (value+ubound)%ubound
  }

  def encode(lst1:ListBuffer[Int], ubound:Int):(ListBuffer[Int],Int)={
    //var lst = ListBuffer[Int]()
    //lst1.foreach(x=>lst+=x)
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

  def decode(deltas:ListBuffer[Int],avg:Int,ubound:Int):ListBuffer[Int]={
    var avgs = new ListBuffer[Int]
    avgs += avg
    var l = 1

    while(deltas.nonEmpty){
      for(i <- 0 to l ){
        val delta = deltas.last
        deltas.remove(deltas.length-1)
        println(deltas)
        val avg = avgs.last
        println(avg)
        avgs.remove(0)
        println(avgs)

        val a = wrap(math.ceil(avg-delta/2.0).toInt,ubound)
        val b = wrap(math.ceil(avg+delta/2.0).toInt,ubound)

        avgs += a
        avgs += b
        println("here" + avgs)
      }
      l*=2
    }
    avgs
  }

  def is_pow2(n:Int):Boolean={
    (n & -n) == n
  }
}
