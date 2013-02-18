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
    
    val MapParser = Parser
}