package org.sisioh.sip.util

import java.security.MessageDigest
import org.sisioh.sip.message.header.SIPConstants


object Utils extends UtilsExt {
  val digesterPoolsSize = 20


  val rand = new java.util.Random(System.nanoTime())

  val digesterPool = (for (q <- 0 until digesterPoolsSize) yield {
    MessageDigest.getInstance("MD5")
  }).toArray


  var counter = 0L

  var callIDCounter: Int = 0


  private val toHex = Array('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')


  def toHexString(b: Array[Byte]) = {
    val c = b.flatMap {
      e =>
        List(
          toHex((e >> 4) & 0x0F),
          toHex(e & 0x0F)
        )
    }
    new String(c)
  }

  val signature = toHexString(Integer.toString(math.abs(rand.nextInt() % 1000)).getBytes)

  def getQuotedString(str: String) = {
    '"' + str.replace("\"", "\\\"") + '"'
  }

  def reduceString(input: String) =
    input.toLowerCase.filterNot(e => e == ' ' || e == '\t')


  def generateCallIdentifier(address: String) = {
    val random = rand.nextLong()
    val hash = math.abs(random % digesterPoolsSize).toInt
    val md = digesterPool(hash)
    md.synchronized {
      val date = (System.nanoTime() + System.currentTimeMillis() + callIDCounter + random).toString
      callIDCounter += 1
      val cid = md.digest(date.getBytes)
      val cidString = Utils.toHexString(cid)
      cidString + "@" + address
    }
  }

  def generateTag() = synchronized {
    Integer.toHexString(rand.nextInt())
  }

  def generateBranchId() = {
    val num = rand.nextLong() + counter + System.currentTimeMillis() + System.nanoTime()
    counter += 1
    val hash = math.abs(num % digesterPoolsSize).toInt
    val digester = digesterPool(hash)
    digester.synchronized {
      val bid = digester.digest(num.toString.getBytes)
      SIPConstants.BRANCH_MAGIC_COOKIE + "-" + this.signature + "-" + Utils.toHexString(bid)
    }
  }
}
