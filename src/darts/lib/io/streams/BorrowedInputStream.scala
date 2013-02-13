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