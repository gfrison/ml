package com.gfrison.ml

import scala.None
/**
 *  k-nearest neighbour implementation for detecting hand-written numbers.
 *
 *  the dataset (60.000 images) comes from http://yann.lecun.com/exdb/mnist/
 *
 * @author Giancarlo Frison <giancarlo@gfrison.com>
 */
object knn extends App {
  //load images
  val images = getImages("train-images.idx3-ubyte", 60000)
  val labels = getLabels("train-labels.idx1-ubyte", 60000)

  //load test data
  val imagestest = getImages("t10k-images.idx3-ubyte", 10000)
  var labelstest = getLabels("t10k-labels.idx1-ubyte", 10000)

  var wrongs = 0
  val totest = 50
  //perform the test on $totest number of images
  for (i <- 0 until totest) {

    /*
     * calculate the distance between test image and dataset (k-NN)
     * sort by relevance and pickup the most occuring result among the top 10 rated
     */
    val disort = {
      for (t <- 0 until images.length) yield { (distance(imagestest(i), images(t)), labels(t)) }
    }.sortBy(_._1)

    val partdisort = for (i <- 0 to 10) yield { disort(i) }
    val highest = Util.highestMultipleFrequency(partdisort)
    highest match {
      case x: Some[(Int, Byte)] => {
        if (x.get._2 == labelstest(i))
          println("correct: " + labelstest(i))
        else {
          println("wrong. detected " + x.get._2 + " instead of " + labelstest(i))
          printImage(imagestest(i))
          wrongs += 1
        }
      }
      case _ => {
        println("not recognized:")
        printImage(imagestest(i))
        wrongs += 1

      }
    }
  }

  println("percentage errors: " + ((wrongs / totest.doubleValue) * 100).toInt + "%")

  /*
   * calculate the distance between 2 images array (just compare blank values '0')
   */
  def distance(img1: Array[Byte], img2: Array[Byte]): Int = {
    var sum = 0
    for (i <- 0 until img1.length)
      if (img1(i) != img2(i)) sum += 1
    sum
  }

  def printImage(img: Array[Byte]) {
    for (x <- 0 until 28) {
      println()
      for (y <- 0 until 28)
        print({ if (img(x * 28 + y) == 0) " " else "*" })
    }
    println()
  }

  def getImages(filename: String, numitem: Int) = {
    val bytes = Util.file2bytes(filename)
    for (i <- 0 until numitem) yield { java.util.Arrays.copyOfRange(bytes, 16 + i * 784, 16 + i * 784 + 784) }
  }

  def getLabels(filename: String, numitem: Int) = {
    var bytes = Util.file2bytes(filename)
    for (i <- 0 until numitem) yield { bytes(8 + i) }
  }

}