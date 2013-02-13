package darts.lib.io.properties

import scala.collection.immutable.{Map => StableMap}
import java.io.{File, InputStream, Reader}
import java.net.{URI, URL}


trait PropertyMap {
	
    def get[T](prop: Property[T]): Option[T]

    def apply[T](prop: Property[T]): T = get(prop) match {
        case Some(value) => value
        case None => throw new IllegalArgumentException("undefined property: " + prop)
    }
}

object PropertyMap {
    
    import darts.lib.io.Utilities._
    
    def apply(map: StableMap[String,String]): PropertyMap = 
        new SimplePropertyMap(map)
    
    def apply(file: File)(implicit config: URLReaderConfiguration): PropertyMap = 
        apply(Parser.parse(file))
    
    def apply(uri: URI)(implicit config: URLReaderConfiguration): PropertyMap = 
        apply(Parser.parse(uri))

    def apply(url: URL)(implicit config: URLReaderConfiguration): PropertyMap = 
        apply(Parser.parse(url))
    
    def apply(source: String, stream: InputStream)(implicit config: URLReaderConfiguration): PropertyMap = 
        apply(Parser.parse(source, stream))
        
    def apply(source: String, reader: Reader): PropertyMap = 
        apply(Parser.parse(source, reader))
}


abstract class BasicPropertyMap 
extends PropertyMap {
    
    import scala.annotation.tailrec
    import java.util.concurrent.atomic.AtomicReference
    
    protected def read(key: String): Option[String]

    private val cache = new AtomicReference[StableMap[Property[_],Any]](StableMap())
    
    protected def clear {
        cache.set(StableMap())
    }
    		
    def get[T](prop: Property[T]): Option[T] = {
        
        @tailrec def intern(old: StableMap[Property[_],Any], parsed: T): Option[T] = old.get(prop) match {
            case Some(present) => Some(present.asInstanceOf[T])
            case None => {
                val aug = old + (prop -> parsed)
                if (cache.compareAndSet(old, aug)) Some(parsed) 
                else intern(cache.get, parsed)
            }
        } 
        
        cache.get.get(prop) match {
            case Some(present) => Some(present.asInstanceOf[T])
            case None => read(prop.path.value) match {
                case None => None
                case Some(string) => intern(cache.get, prop.descriptor.parseValue(string))
            }
        }
    }
}

class SimplePropertyMap (val underlying: StableMap[String,String])
extends BasicPropertyMap {
    
	protected def read(key: String): Option[String] = underlying.get(key)
}