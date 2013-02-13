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