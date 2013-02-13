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
