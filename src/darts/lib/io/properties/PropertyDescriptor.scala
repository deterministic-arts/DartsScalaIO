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

package darts.lib.io.properties

import java.io.{File => JFile}

trait PropertyDescriptor[T] {
    
    def parseValue(str: String): T
}

object PropertyDescriptor {

	private val BoolMap = Map(
        "true" -> true, "yes" -> true, "1" -> true, "on" -> true, "enabled" -> true,
        "false" -> false, "no" -> false, "0" -> false, "off" -> false, "disabled" -> false
    )
    
	implicit final case object String extends PropertyDescriptor[String] {
	    def parseValue(str: String): String = str
	}
    
	implicit final case object Byte extends PropertyDescriptor[Byte] {
	    def parseValue(str: String): Byte = str.toByte
	}
	
	implicit final case object Short extends PropertyDescriptor[Short] {
	    def parseValue(str: String): Short = str.toShort
	}
	
	implicit final case object Integer extends PropertyDescriptor[Int] {
	    def parseValue(str: String): Int = str.toInt
	}
    
	implicit final case object Long extends PropertyDescriptor[Long] {
	    def parseValue(str: String): Long = str.toLong
	}
	
	implicit final case object Float extends PropertyDescriptor[Float] {
	    def parseValue(str: String): Float = str.toFloat
	}
	
	implicit final case object Double extends PropertyDescriptor[Double] {
	    def parseValue(str: String): Double = str.toDouble
	}
	
	implicit final case object BigDecimal extends PropertyDescriptor[BigDecimal] {
	    def parseValue(str: String): BigDecimal = scala.BigDecimal(str)
	}
	
	implicit final case object Boolean extends PropertyDescriptor[Boolean] {
	    def parseValue(str: String): Boolean = BoolMap(str.toLowerCase)
	}
	
	implicit final case object File extends PropertyDescriptor[JFile] {
	    def parseValue(str: String): JFile = new JFile(str)
	}
}