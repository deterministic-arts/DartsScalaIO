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

import scala.annotation.tailrec
import java.io._
import java.security._
import javax.crypto._
import javax.crypto.spec._
import darts.lib.io.ByteString

object AuthenticatingInputStream {
    
    def apply(stream: InputStream, algorithm: String, key: Key): AuthenticatingInputStream = {
	    val mac = {
	        val m = Mac.getInstance(algorithm)
	        m.init(key)
	        m
	    }
	    new AuthenticatingInputStream(stream, mac)
    }
}

/**
 * Input stream, which wraps around some other input stream, 
 * computing a MAC from the data, which is read from it. The 
 * MAC is computed only from bytes actually read from the authenticating 
 * stream, i.e., if the authenticating stream is closed before 
 * the underlying stream is exhausted, only data, which has 
 * been read by the application is considered in the generated 
 * MAC value.
 * 
 * This kind of input stream does not support Java's `mark`
 * feature.
 */

final class AuthenticatingInputStream private (private val underlying: InputStream, private val mac: Mac) 
extends InputStream {
	
    private val buffer = new Array[Byte](512)
    private var fill: Int = 0
    private var pointer: Int = 0
    private var done: Boolean = false
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
        
    def bytesRead: Long =
        totalCount
    
    private def fillBuffer {
            
        if (!done) {

            assert(pointer == fill)

            if (fill > 0) mac.update(buffer, 0, fill)

            @tailrec def loop(got: Int) {
            	if (got < 0) done = true
            	else if (got == 0) loop(underlying.read(buffer))
            	else fill = got
            }
            
            pointer = 0
            fill = 0
            loop(underlying.read(buffer))
        }
    }

    @throws(classOf[IOException])
    override def close(): Unit = {
        if (answer == null) {
        	if (!done) {
        	    if (pointer > 0) mac.update(buffer, 0, pointer)
        	    done = true
        	    pointer = 0
        	    fill = 0
        	}
            answer = ByteString(mac.doFinal())
        }
        underlying.close
    }
    
    @throws(classOf[IOException])
    override def read(): Int = {
        @tailrec def loop: Int = {
	        if (pointer < fill) {
	            val ch = buffer(pointer) & 0xff
	            pointer += 1
	            totalCount += 1
	            ch
	        } else if (done) -1
	        else {
	            fillBuffer
	            loop
	        }
    	}
        loop
    }
}