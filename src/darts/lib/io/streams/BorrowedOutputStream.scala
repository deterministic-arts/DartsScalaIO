package darts.lib.io.streams

import java.io._

object BorrowedOutputStream {
    
    def apply(stream: OutputStream): BorrowedOutputStream = new BorrowedOutputStream(stream)
}

final class BorrowedOutputStream private (private val underlying: OutputStream) 
extends OutputStream {
    
    @throws(classOf[IOException])
    override def close(): Unit = ()
    
    @throws(classOf[IOException])
    override def flush(): Unit = underlying.flush

    @throws(classOf[IOException])
    override def write(byte: Int): Unit = underlying.write(byte)
    
    @throws(classOf[IOException])
    override def write(bytes: Array[Byte]): Unit = underlying.write(bytes)
    
    @throws(classOf[IOException])
    override def write(bytes: Array[Byte], offs: Int, len: Int): Unit = underlying.write(bytes, offs, len)
}