//-----------------------------------------------------------------------------//
//                                                                             //
//  Deterministic Arts - Scala I/O Utitilites								   //
//  Copyright (c) 2012 Dirk Eßer                                               //
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

import java.io._

object ChunkingInputStream {
    
    def apply(stream: InputStream): ChunkingInputStream = new ChunkingInputStream(stream)
}

final class ChunkingInputStream private (private val underlying: InputStream)
extends InputStream {
    
    private var remaining: Int = 0
    private var done: Boolean = false
    
    private def mustRead: Int = {
	    val byte = underlying.read
	    if (byte < 0) throw new IOException
	    byte & 0xff
	}
    
    private def readInt: Int = {
		val d1 = mustRead 
		val d2 = mustRead << 8
		val d3 = mustRead << 16
		val d4 = mustRead << 24
		val cn = d1 | d2 | d3 | d4
		cn
    }
    
	@throws(classOf[IOException])
    override def close(): Unit = ()
    
    @throws(classOf[IOException])
    override def read(): Int = {
	    if (remaining > 0) {
	        remaining -= 1
	        mustRead
	    } else if (!done) {
	        val r = readInt
	        if (r < 0) throw new IOException
	        else if (r > 0) { remaining = r; read }
	        else {
	            done = true
	            -1
	        }
	    } else 
	        -1
	}
    
	@throws(classOf[IOException])
	override def read(buf: Array[Byte], start: Int, length: Int): Int = {
	    if (length > remaining) super.read(buf, start, length)
	    else {
	        val read = underlying.read(buf, start, length)
	        if (read > 0) remaining -= read
	        read
	    } 
	}
	
    @throws(classOf[IOException])
    override def available(): Int = remaining
}