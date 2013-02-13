package darts.lib.io.streams

import scala.annotation.tailrec
import java.io._

object ChunkingOutputStream {
    
    def apply(stream: OutputStream): ChunkingOutputStream = apply(stream, 1024)
    def apply(stream: OutputStream, size: Int): ChunkingOutputStream = new ChunkingOutputStream(stream, size)
}

final class ChunkingOutputStream private (private val underlying: OutputStream, val chunkSize: Int) 
extends OutputStream {
    
    assert(chunkSize >= 8)

    private val buffer = new Array[Byte](chunkSize)
    private var written: Int = 0
    private var closed: Boolean = false

    private def writeInt(value: Int) {
        var v = value
        underlying.write(v & 0xff); v >>= 8
        underlying.write(v & 0xff); v >>= 8
        underlying.write(v & 0xff); v >>= 8
        underlying.write(v & 0xff)
    }
    
    private def flushChunk(done: Boolean) {
        if (written > 0) {
            writeInt(written)
            underlying.write(buffer, 0, written)
            written = 0
        }
        if (done) writeInt(0)
    }
    
    @throws(classOf[IOException])
    override def close(): Unit = {
        if (!closed) {
            closed = true
	        flushChunk(true)
	        underlying.close
        }
    }
    
    @throws(classOf[IOException])
    override def flush(): Unit = {
        flushChunk(false)
        underlying.flush
    }

    @throws(classOf[IOException])
    override def write(byte: Int): Unit = {
        if (written == chunkSize) flushChunk(false)
        buffer(written) = (0xff & byte).asInstanceOf[Byte]
        written += 1
    }
}