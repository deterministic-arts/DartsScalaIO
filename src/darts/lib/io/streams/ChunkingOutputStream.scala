//-----------------------------------------------------------------------------//
//                                                                             //
//  Deterministic Arts - Scala I/O Utitilites								   //
//  Copyright (c) 2010 Dirk Eßer                                               //
//                                                                             //
//  Licensed under the Apache License, Version 2.0 (the "License");            //
//  you may not use this file except in compliance with the License.           //
//  You may obtain a copy of the License at                                    //
//                                                                             //
//    http://www.apache.org/licenses/LICENSE-2.0                               //
//                                                                             //
//  Unless required by applicable law or agreed to in writing, software        //
//  distributed under the License is distributed on an "AS IS" BASIS,          //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   //
//  See the License for the specific language governing permissions and        //
//  limitations under the License.                                             //
//                                                                             //
//-----------------------------------------------------------------------------//

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