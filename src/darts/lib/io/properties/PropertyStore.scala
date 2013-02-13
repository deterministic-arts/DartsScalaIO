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

import scala.collection.immutable.{Map => StableMap}
import java.io.File


trait PropertyStore {
	
    def properties: PropertyMap
}

object PropertyStore {

    def apply(map: StableMap[String,String]): PropertyStore = new MemoryPropertyStore(map)
    def apply(map: PropertyMap): PropertyStore = new MemoryPropertyStore(map)
    def apply(file: File, encoding: String): PropertyStore = new SimpleFilePropertyStore(file, encoding)
    def apply(file: File): PropertyStore = new SimpleFilePropertyStore(file)
}


class MemoryPropertyStore (val properties: PropertyMap)
extends PropertyStore {

    def this(map: StableMap[String,String]) = this(PropertyMap(map))
}


class SimpleFilePropertyStore (val file: File, val encoding: String)
extends PropertyStore {
    
    import darts.lib.io.Utilities
    import Utilities.URLReaderConfiguration
    
    def this(file: File) = this(file, "utf-8")
    
    protected def urlReaderConfiguration: URLReaderConfiguration = Utilities.defaultURLReaderConfiguration
    def properties: PropertyMap = propmap
    
    private val propmap = new Mapping
    private val mutex = new AnyRef
	private var base: StableMap[String,String] = null
	private var ftime: Long = java.lang.Long.MIN_VALUE

	private def readAll: StableMap[String,String] = mutex synchronized {
        if (base ne null) base else {
            implicit val config = urlReaderConfiguration
            ftime = file.lastModified()
        	base = Parser.parse(file)
        	base
        }
    }

    def check: Boolean = {
        if (ftime >= file.lastModified()) false 
        else { reset; true }
    }

    def reset {
        mutex synchronized {
            base = null
            propmap.reset
        }
    }
        
    private class Mapping extends BasicPropertyMap {

        private[SimpleFilePropertyStore] def reset: Unit =
            clear
        
	    protected def read(key: String): Option[String] =
	        readAll.get(key)    
    }
}