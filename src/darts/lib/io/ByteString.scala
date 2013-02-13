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

final class ByteString private (private val data: Array[Byte])
extends Traversable[Byte] with Ordered[ByteString] {
    
    private var hash: Int = 0
    
    private def compare(array1: Array[Byte], array2: Array[Byte]): Int = {
        val len1 = array1.length
        val len2 = array2.length
        val mlen = if (len1 < len2) len1 else len2
        var p: Int = 0
        while (p < mlen) {
            val b1 = 0xff & array1(p)
            val b2 = 0xff & array2(p)
            val df = b1 - b2
            if (df != 0) return df
            p += 1
        }
        if (len1 < len2) -1 else if (len1 > len2) 1 else 0
    }
    
    override def compare(oth: ByteString): Int =
        compare(data, oth.data)
    
    override def equals(ob: Any): Boolean = ob match {
        case b: ByteString => compare(data, b.data) == 0
        case _ => false
    }
    
    override def hashCode: Int = {
        var h = hash
        if (h == 0) {
        	val length = data.length
        	var p: Int = 0
        	while (p < length) {
        	    h = h * 31 + (0xff & data(p))
        	    p = p + 1
        	}
            if (h == 0) h = 1
            hash = h
        }
        h
    }
    
    def apply(index: Int): Byte = data(index)
    def length: Int = data.length
    override def size: Int = data.length
    def foreach[U](fn: Byte=>U): Unit = data.foreach(fn)
    def toString(enc: String): String = new String(data, enc)

    def substring(start: Int): ByteString = 
        substring(start, length)
    
    def substring(start: Int, end: Int): ByteString = 
        ByteString.apply(data, start, end)
        
    def indexOf(byte: Byte): Int =
        indexOf(byte & 0xff, 0)
    
    def indexOf(byte: Byte, start: Int): Int =
        indexOf(byte & 0xff, start)
        
    def indexOf(byte: Int): Int =
        indexOf(byte, 0)

    def indexOf(byte: Int, start: Int): Int = {
        val length = data.length
        var p: Int = start
        while (p < length) {
            if ((0xff & data(p)) == byte) return p
            p += 1
        }
        -1
    }

    def lastIndexOf(byte: Byte, start: Int): Int = 
        lastIndexOf(0xff & byte, start)
    
    def lastIndexOf(byte: Byte): Int = 
        lastIndexOf(0xff & byte, length)
    
    def lastIndexOf(byte: Int): Int = 
        lastIndexOf(byte, length)
    
    def lastIndexOf(byte: Int, start: Int): Int = {
        var p: Int = start - 1
        while (start >= 0) {
        	if ((0xff & data(p)) == byte) return p
        	p -= 1
        }
        -1
    }
    
    override def toString: String =
    	ByteArray.toHexString(data, 0, data.length)
    	
	def toByteArray: Array[Byte] = toByteArray(0, data.length)
	def toByteArray(start: Int): Array[Byte] = toByteArray(start, data.length)
    def toByteArray(start: Int, end: Int): Array[Byte] = {
        val copy = new Array[Byte](end - start)
        System.arraycopy(data, start, copy, 0, end - start)
        copy
    }
    
    

    def writeTo(stream: OutputStream): Unit = writeTo(stream, 0, data.length)
    def writeTo(stream: OutputStream, start: Int): Unit = writeTo(stream, start, data.length - start)
    def writeTo(stream: OutputStream, start: Int, length: Int): Unit = 
        if (start < 0 || start + length > data.length) throw new IndexOutOfBoundsException
        else stream.write(toByteArray(start, start + length))
}

final object ByteString {
    
    val Empty = new ByteString(ByteArray.Empty)
    
    def apply(data: Array[Byte], start: Int, end: Int): ByteString = {
        if (data.length == 0) Empty else {
            val length = end - start
            val copy = new Array[Byte](length)
            System.arraycopy(data, start, copy, 0, length)
            new ByteString(copy)
        }
    }
    
    def apply(data: Array[Byte], start: Int): ByteString = 
        apply(data, start, data.length)
        
    def apply(data: Array[Byte]): ByteString = 
        apply(data, 0, data.length)
    
    def apply(str: String): ByteString =
        if (str.length == 0) Empty else new ByteString(ByteArray.fromHexString(str))
    
    def apply(str: String, enc: String): ByteString = {
        val data = str.getBytes(enc)
        if (data.length == 0) Empty else new ByteString(data)
    }
    
    def readFrom(stream: InputStream, length: Int): Option[ByteString] = {
        val buffer = new Array[Byte](length)
        @tailrec def loop(offs: Int): Option[ByteString] = {
            if (offs == length) Some(new ByteString(buffer))
            else {
            	val count = stream.read(buffer, offs, length - offs)
            	if (count >= 0) loop(offs + count)
            	else if (offs > 0) Some(apply(buffer, 0, offs))
        	    else None
            }
        }
        loop(0)
    }
    
    def mustReadFrom(stream: InputStream, length: Int): ByteString = {
        val buffer = new Array[Byte](length)
        @tailrec def loop(offs: Int): ByteString = {
            if (offs == length) new ByteString(buffer)
            else {
            	val count = stream.read(buffer, offs, length - offs)
            	if (count >= 0) loop(offs + count)
            	else throw new IOException("not enough data")
            }
        }
        loop(0)
    }
}

final object ByteArray {
 
    val Empty = new Array[Byte](0)
        
    def fromHexString(str: String): Array[Byte] = 
        fromHexString(str, 0, str.length())
    
    def fromHexString(str: String, start: Int): Array[Byte] = 
        fromHexString(str, start, str.length())
    
    def fromHexString(str: String, start: Int, end: Int): Array[Byte] = {
        
        val length = end - start
        
        if (length % 2 != 0) throw new IllegalArgumentException
        else if (length == 0) Empty 
        else {
        	
            val array = new Array[Byte](length / 2)
            var rp: Int = start
            var wp: Int = 0
            
            while (rp < end) {
                
                val d1 = Character.digit(str.charAt(rp), 16); rp += 1
                val d2 = Character.digit(str.charAt(rp), 16); rp += 1
                
                if (d1 < 0 || d2 < 0) throw new IllegalArgumentException
                else {
                    
                    val byte = (d1 << 4) | d2
                    array(wp) = byte.asInstanceOf[Byte]
                    wp += 1
                }
            }
            
            array
        }
    }
    
    def toHexString(data: Array[Byte], start: Int, end: Int): String = {
        val digits = "0123456789abcdef"
        val length = end - start
        val buffer = new Array[Char](2 * length)
        var rp: Int = start
        var wp: Int = 0
        while (rp < end) {
            val byte = 0xff & data(rp)
            rp += 1
            buffer(wp) = digits.charAt(byte >>> 4); wp += 1
            buffer(wp) = digits.charAt(byte & 0xf); wp += 1
        }
        new String(buffer)
    }
    
    def toHexString(data: Array[Byte], start: Int): String = 
        toHexString(data, start, data.length)
        
    def toHexString(data: Array[Byte]): String = 
        toHexString(data, 0, data.length)
}