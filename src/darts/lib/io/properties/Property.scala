package darts.lib.io.properties

final case class Property[T] (val path: PropertyPath)(implicit val descriptor: PropertyDescriptor[T]) {

}

object Property {
    
    type Path = PropertyPath
    val Path = PropertyPath
    
    type Descriptor[T] = PropertyDescriptor[T]
    val Descriptor = PropertyDescriptor
    
    type Mapping = PropertyMap
    val Mapping = PropertyMap
    
    type Group = PropertyGroup
    
    type Store = PropertyStore
    val Store = PropertyStore
}