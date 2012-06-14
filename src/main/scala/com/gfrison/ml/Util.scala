package com.gfrison.ml

import java.io.FileInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.net.URL
import java.net.URLConnection
import java.io.InputStream
import java.io.BufferedInputStream
import org.apache.commons.io.IOUtils

/**
 * @author Giancarlo Frison <giancarlo@gfrison.com>
 *
 */
object Util {
  def file2bytes(filename: String): Array[Byte] = IOUtils.toByteArray(getClass.getResourceAsStream("/" + filename))

  //http://skipoleschris.blogspot.com/2010/11/functional-programming-challenge-most.html
  def highestMultipleFrequency[T](items: IndexedSeq[T]): Option[T] = {
    type Frequencies = Map[T, Int]
    type Frequency = Pair[T, Int]

    def freq(acc: Frequencies, item: T) = acc.contains(item) match {
      case true => acc + Pair(item, acc(item) + 1)
      case _ => acc + Pair(item, 1)
    }
    def mostFrequent(acc: Option[Frequency], item: Frequency) = acc match {
      case None if item._2 >= 2 => Some(item)
      case Some((value, count)) if item._2 > count => Some(item)
      case _ => acc
    }
    items.foldLeft(Map[T, Int]())(freq).foldLeft[Option[Frequency]](None)(mostFrequent) match {
      case Some((value, count)) => Some(value)
      case _ => None
    }
  }

  /*
   * http://www.java2s.com/Tutorial/Java/0320__Network/SavebinaryfilefromURL.htm
   */
  def saveFileFromUrl(url: String, dest: String) = {
    val u = new URL(url);
    val uc = u.openConnection();
    val contentType = uc.getContentType();
    val contentLength = uc.getContentLength();
    if (contentType.startsWith("text/") || contentLength == -1) {
      throw new IOException("This is not a binary file.");
    }
    val raw = uc.getInputStream();
    val in = new BufferedInputStream(raw);
    val data = new Array[Byte](contentLength)
    var bytesRead = 0;
    var offset = 0;
    while (offset < contentLength && bytesRead != -1) {
      bytesRead = in.read(data, offset, data.length - offset)
      offset += bytesRead;
    }
    in.close();

    if (offset != contentLength) {
      throw new IOException("Only read " + offset + " bytes; Expected " + contentLength + " bytes");
    }

    var filename = u.getFile().substring(url.lastIndexOf('/') + 1);
    var out = new FileOutputStream(filename);
    out.write(data);
    out.flush();
    out.close();
  }
}