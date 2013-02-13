package darts.lib.io.streams

import java.io._

object BorrowedInputStream {
    
    def apply(stream: InputStream): BorrowedInputStream = new BorrowedInputStream(stream)
}

final class BorrowedInputStream private (private val underlying: InputStream)
extends InputStream {
    
    @throws(classOf[IOException])
    override def close(): Unit = ()
    
    @throws(classOf[IOException])
    override def read(): Int = underlying.read
    
    @throws(classOf[IOException])
    override def read(buffer: Array[Byte]): Int = underlying.read(buffer)
    
    @throws(classOf[IOException])
    override def read(buffer: Array[Byte], offs: Int, len: Int): Int = underlying.read(buffer, offs, len)
    
    @throws(classOf[IOException])
    override def available(): Int = underlying.available()
    
    @throws(classOf[IOException])
    override def markSupported(): Boolean = underlying.markSupported()
    
    @throws(classOf[IOException])
    override def mark(limit: Int): Unit = underlying.mark(limit)
    
    @throws(classOf[IOException])
    override def reset(): Unit = underlying.reset()
    
    @throws(classOf[IOException])
    override def skip(num: Long): Long = underlying.skip(num)
}