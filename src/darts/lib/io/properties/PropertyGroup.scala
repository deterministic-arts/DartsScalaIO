package darts.lib.io.properties

abstract class PropertyGroup (val root: PropertyPath) {
	
    protected def leaf[T](seg: String)(implicit desc: PropertyDescriptor[T]): Property[T] =
        root.child(seg).property
}