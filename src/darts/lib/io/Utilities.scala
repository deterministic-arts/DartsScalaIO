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

import java.util.regex.Pattern
import java.net._
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
    
    private val ConnectTimeOut = 30000
    private val ReadTimeOut = 30000
    private val DefaultCharSet = "UTF-8"
    private val CharSetPattern = Pattern.compile(".*;\\s*[cC][hH][aA][rR][sS][eE][tT]\\s*=\\s*([^\\s;]+)\\s*(?:;.*)?$")
    
    final case class URLReaderConfiguration (
        val encoding: String,
        val readTimeout: Int,
        val connectTimeout: Int
    ) {
        def withEncoding(e: String): URLReaderConfiguration = 
            if (e == encoding) this 
            else URLReaderConfiguration(e, readTimeout, connectTimeout)
            
        def withReadTimeout(millis: Int): URLReaderConfiguration = 
            if (millis == readTimeout) this
            else URLReaderConfiguration(encoding, millis, connectTimeout)
            
        def withConnectTimeout(millis: Int): URLReaderConfiguration =
            if (millis == connectTimeout) this
            else URLReaderConfiguration(encoding, readTimeout, millis)
    }
    
    implicit val defaultURLReaderConfiguration = URLReaderConfiguration(
        DefaultCharSet,
        ReadTimeOut,
        ConnectTimeOut
    )
    
    private def coalesce[S <: AnyRef](v1: S, v2: =>S): S = if (v1 == null) v2 else v1
    
    private def extractCharSet(connection: URLConnection, default: =>String): String = {
        extractCharSet(coalesce(connection.getContentType, "application/octet-stream"), default)
    }
    
    private def extractCharSet(ct: String, default: =>String): String = {
        val matcher = CharSetPattern.matcher(ct)
        if (!matcher.matches()) default
        else {
            val name = matcher.group(1).trim
            if (name.length == 0) default
            else name
        }
    }
    
    def withURLStream[U](url: URL)(fn: (String,InputStream,String)=>U)(implicit config: URLReaderConfiguration): U = {
        
        val uri = url.toExternalForm
		val connection = url.openConnection()
		
		connection.setDoInput(true)
		connection.setDoOutput(false)
		connection.setAllowUserInteraction(false)
		connection.setUseCaches(false)
		connection.setConnectTimeout(config.connectTimeout)
		connection.setReadTimeout(config.readTimeout)

		val stream = connection.getInputStream

		try fn(uri, stream, extractCharSet(connection, config.encoding)) 
		finally
			stream.close
    }
    
    def withURLReader[U](url: URL)(fn: Reader=>U)(implicit config: URLReaderConfiguration): U = {
		withURLStream(url)({ (uri, stream, encoding) =>
		    val reader = new InputStreamReader(stream, encoding)
		    try fn(reader)
		    finally
		    	reader.close
		})(config)
    }
}