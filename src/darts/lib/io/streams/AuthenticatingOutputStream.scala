package darts.lib.io.streams

import scala.annotation.tailrec
import java.io._
import java.security._
import javax.crypto._
import javax.crypto.spec._
import darts.lib.io.ByteString

object AuthenticatingOutputStream {
    
    def apply(stream: OutputStream, algo: String, key: Key): AuthenticatingOutputStream = {
        val mac = {
    		val m = Mac.getInstance(algo)
			m.init(key)
			m
        }
        new AuthenticatingOutputStream(stream, mac)
    }
}

/**
 * Authenticating output stream, which wraps around another
 * output stream, computing a MAC from all data written into 
 * it. The MAC value is available after the stream has been 
 * closed.
 */

final class AuthenticatingOutputStream private (private val underlying: OutputStream, private val mac: Mac) 
extends OutputStream {
	
    private val buffer = new Array[Byte](512)
    private var pointer: Int = 0
    private var answer: ByteString = null
    private var totalCount: Long = 0L
    
    /**
     * Yields the generated MAC code as byte string. Note,
     * that the value of this property is undefined unless
     * the stream has been closed; the current implementation
     * signals an exception, if this method is called before
     * the stream has been closed.
     * 
     * @return the computed MAC value as `ByteString`.
     */

    def result: ByteString =
        if (answer == null) throw new IllegalStateException
        else answer
        
    def bytesWritten: Long = totalCount
        
    private def done: Boolean = answer != null
        
    @throws(classOf[IOException])
    override def flush(): Unit = {
		if (!done) {
		    if (pointer > 0) {
		        underlying.write(buffer, 0, pointer)
		        mac.update(buffer, 0, pointer)
		        totalCount += pointer
		        pointer = 0
		    }
		    underlying.flush
		}
    }
    
    @throws(classOf[IOException])
    override def close(): Unit = {
		if (!done) {
		    if (pointer > 0) {
		        underlying.write(buffer, 0, pointer)
		        mac.update(buffer, 0, pointer)
		        totalCount += pointer
    		    pointer = 0
		    }
		    answer = ByteString(mac.doFinal())
		}
		underlying.close
    }
    
    @throws(classOf[IOException])
    override def write(byte: Int): Unit = {
    	if (pointer == buffer.length) flush()
    	buffer(pointer) = (0xff & byte).asInstanceOf[Byte]
        pointer += 1
    }
}
