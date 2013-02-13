package darts.lib.io.properties

final class PropertyPath private (val value: String) 
extends Ordered[PropertyPath] {

    import PropertyPath._
    
    def child(seg: String): PropertyPath = 
        new PropertyPath(value + "." + checkSegment(seg))
    
    override def equals(ob: Any): Boolean = ob match {
        case p: PropertyPath => value == p.value
        case _ => false
    }

    override def hashCode: Int = value.hashCode
    override def toString: String = "Path(" + value + ")"
    override def compare(p: PropertyPath): Int = value compareTo p.value

    lazy val (parent: Option[PropertyPath], name: String) = {
        val dot = value.lastIndexOf('.')
        if (dot < 0) (None, value) else {
            val prefix = value.substring(0, dot)
            val suffix = value.substring(1 + dot)
            (Some(PropertyPath(prefix)), suffix)
        }
    }

    def isDescendantOf(oth: PropertyPath): Boolean = isDescendantOf(oth, false)
    def isDescendantOf(oth: PropertyPath, strict: Boolean): Boolean = {
        val llen = value.length
        val olen = oth.value.length
        if (llen < olen) false
        else if (llen == olen) !strict && value == oth.value
        else value.charAt(olen) == '.' && value.regionMatches(0, oth.value, 0, olen)
    }
    
    def property[T](implicit desc: PropertyDescriptor[T]): Property[T] = 
        Property(this)

    def + (path: PropertyPath): PropertyPath = 
        new PropertyPath(value + "." + path.value)
    
    def + (path: String): PropertyPath =
        new PropertyPath(value + "." + checkPath(path))
}

final object PropertyPath {
    
    import java.util.regex.Pattern
    
    private val SegPattern = Pattern.compile("^[a-zA-Z$_](-?[a-zA-Z0-9$_]+)*$")
    private val PathPattern = Pattern.compile("^[a-zA-Z$_](-?[a-zA-Z0-9$_]+)*([.][a-zA-Z$_](-?[a-zA-Z0-9$_]+)*)*$")
    
    private def checkSegment(str: String): String = 
        if (SegPattern.matcher(str).matches()) str 
        else throw new IllegalArgumentException

    private def checkPath(str: String): String =
        if (PathPattern.matcher(str).matches()) str
        else throw new IllegalArgumentException

    implicit def propertyPathFromString(str: String): PropertyPath = apply(str)
        
    def apply(path: String): PropertyPath = 
        new PropertyPath(checkPath(path))
        
    def unapply(ob: Any): Option[String] = ob match {
        case p: PropertyPath => Some(p.value)
        case _ => None
    }
    
    final object Root {
        def apply(p: String): PropertyPath = new PropertyPath(checkSegment(p))
        def unapply(p: PropertyPath): Option[String] = {
            val d = p.value.lastIndexOf('.')
            if (d < 0) Some(p.value) else None 
        }
    }
    
    final object Segment {
        def apply(p: PropertyPath, s: String): PropertyPath = p.child(s)
        def unapply(p: PropertyPath): Option[(PropertyPath,String)] = {
            val d = p.value.lastIndexOf('.')
            if (d < 0) None else Some(new PropertyPath(p.value.substring(0, d)), p.value.substring(1 + d))
        }
    }
}
