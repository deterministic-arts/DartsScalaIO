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

package darts.lib.io

import scala.annotation.tailrec
import java.io._

object Utilities {
	
    private val BufSize = 8192

    def copyData(src: InputStream, dst: OutputStream): Long = {
        
        val buffer = new Array[Byte](BufSize)
        
    	@tailrec def loop(got: Int, accu: Long): Long = {
            if (got < 0) accu
            else if (got == 0) loop(src.read(buffer), accu)
            else {
                dst.write(buffer, 0, got)
                loop(src.read(buffer), accu + got)
            }
        }
        
        loop(src.read(buffer), 0L)
    }
    
    def readAll(src: InputStream, buffer: Array[Byte]): Int = 
        readAll(src, buffer, 0, buffer.length)
    
    def readAll(src: InputStream, buffer: Array[Byte], start: Int): Int = 
        readAll(src, buffer, start, buffer.length - start)
    
    def readAll(src: InputStream, buffer: Array[Byte], start: Int, length: Int): Int = {
        @tailrec def loop(start: Int, remaining: Int): Int = {
            if (remaining == 0) length
            else {
            	val got = src.read(buffer, start, remaining)
            	if (got < 0) length - remaining
            	else loop(start + got, remaining - got)
            }
        }
        loop(start, length)
    }
}