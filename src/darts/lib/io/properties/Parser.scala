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

import java.io._
import java.net._

import scala.annotation.tailrec

final case class Location (
    val source: String,
    val offset: Int,
    val line: Int
) {
    override def toString: String = 
        source + "[" + offset + "]:" + line
}

private sealed trait Token {
    def location: Location
}

private object Token {

    final case class Error (val message: String, val location: Location) extends Token
    final case class EndOfInput (val location: Location) extends Token
    final case class Word (val value: String, val location: Location) extends Token
    final case class Literal (val value: String, val location: Location) extends Token
    final case class OpeningBrace (val location: Location) extends Token
    final case class ClosingBrace (val location: Location) extends Token
    final case class Equals (val location: Location) extends Token
    final case class Period (val location: Location) extends Token
}


private class Lexer (val source: String, val reader: Reader) {
	
	private var buffer: Int = -1
	private var offset: Int = 0
	private var line: Int = 1
	private var eof: Boolean = false
	private var locOpt: Option[Location] = None
	private var tokVar: Token = Token.Error("invalid state", Location(source, 0, 1))
	
	def token: Token = tokVar
	def location: Location = { 
	    if (locOpt.isEmpty) locOpt = Some(Location(source, offset, line))
	    locOpt.get
	}
	
	private def peek: Int = {
	    if (buffer >= 0) buffer
	    else {
	        val ch = reader.read
	        if (ch < 0) -1
	        else {
	            buffer = ch
	            ch
	        }
	    }
	}
	
	private def read: Int = {
	    var ch: Int = buffer
	    if (ch >= 0) 
	        buffer = -1
	    else
	        ch = reader.read
        if (ch < 0) -1
        else {
            offset += 1
            locOpt = None
            ch match {
                case '\n' => { line += 1; ch }
                case _ => ch
            }
        }
	}
	
	private def produce(form: =>Token) {
	    val token = form
	    read
	    tokVar = token
	}
	
	def advance {
		
	    if (!eof) {

	        @tailrec def skipToEol {
	            val ch = read
	            if (ch >= 0 && ch != '\n') skipToEol
	        }
	        
	        def isWordStart(ch: Int): Boolean = 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '$' || ch == '_' 
            def isWordChar(ch: Int): Boolean = isWordStart(ch) || '0' <= ch && ch <= '9' || ch == '-'
	        
            def readNumber {
	            
	            val BeforeSign = 0
	            val AfterSign = 1
	            val IntegerPart = 2
	            val AfterDot = 3
	            val FractionPart = 4
	            val AfterExpMarker = 5
	            val AfterExpSign = 6
	            val ExponentPart = 7
	            
	            val start = location
	            val buffer = new StringBuilder
	            
	            def add {
	                buffer += read.asInstanceOf[Char]
	            }
	            
	            def badSyntax = Token.Error("unsupported number syntax", start)
	            
	            @tailrec def loop(ch: Int, mode: Int): Token = {
	                if (ch < 0) mode match {
	                    case IntegerPart | FractionPart | ExponentPart => Token.Literal(buffer.result(), start)
	                    case _ => badSyntax
	                } else ch match {
	                    case '+' | '-' => mode match {
	                        case BeforeSign => { add; loop(peek, AfterSign) }
	                        case AfterExpMarker => { add; loop(peek, AfterExpSign) }
	                        case _ => loop(-1, mode)
	                    }
	                    case '.' => mode match {
	                        case IntegerPart => { add; loop(peek, AfterDot) }
	                        case _ => loop(-1, mode)
	                    }
	                    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => mode match {
	                        case BeforeSign | AfterSign | IntegerPart => { add; loop(peek, IntegerPart) }
	                        case AfterDot | FractionPart => { add; loop(peek, FractionPart) }
	                        case AfterExpMarker | AfterExpSign | ExponentPart => { add; loop(peek, ExponentPart) }
	                        case _ => loop(-1, mode)
	                    }
	                    case 'e' | 'E' => mode match {
	                        case IntegerPart => { read; buffer ++= ".0e"; loop(peek, AfterExpMarker) }
	                        case FractionPart => { add; loop(peek, AfterExpMarker) }
	                        case _ => loop(-1, mode)
	                    }
	                    case _ => loop(-1, mode)
	                }
	            }
	            
	            tokVar = loop(peek, BeforeSign)
	        }
                
	        def readWord {
	            
	            val Initial = 0
	            val AfterLetter = 1
	            val AfterSeparator = 2
	            
	            val start = location
	            val buffer = new StringBuilder
	            
	            def add {
	            	buffer += read.asInstanceOf[Char]
	            }
	            
	            @tailrec def loop(ch: Int, mode: Int) {
	                if (ch < 0) {
	                    mode match {
	                        case AfterLetter => tokVar = Token.Word(buffer.result(), start)
	                        case AfterSeparator => tokVar = Token.Error("invalid word (separator must be followed by a letter or digit)", start)
	                        case Initial => throw new AssertionError
	                    }
	                } else {
	                	ch match {
	                	    case wch if 'a' <= wch && wch <= 'z' => { add; loop(peek, AfterLetter) } 
	                	    case wch if 'A' <= wch && wch <= 'Z' => { add; loop(peek, AfterLetter) }
	                	    case '_' | '$' => { add; loop(peek, AfterLetter) }
	                	    case wch if '0' <= wch && wch <= '9' => mode match {
	                	        case AfterLetter | AfterSeparator => { add; loop(peek, AfterLetter) }
	                	        case Initial => throw new AssertionError
	                	    }
	                	    case '-' => mode match {
	                	        case AfterLetter => { add; loop(peek, AfterSeparator) }
	                	        case AfterSeparator => loop(-1, mode)
	                	        case Initial => throw new AssertionError
	                	    }
	                	    case _ => loop(-1, mode)
	                	}
	                }
	            }
	            
	            loop(peek, Initial)
	        }
	        
	        def readLiteral {

	            val start = location
	            val buffer = new StringBuilder
	            
	            read	// Skip leading '"'
	            
	            @tailrec def loop(ch: Int) {
	                
	                if (ch < 0) {
	                    
	                    tokVar = Token.Error("unterminated string literal", location)
	                    
	                } else {
	                    
	                	if (ch == '"') tokVar = Token.Literal(buffer.result(), start)
	                	else if (ch != '\\') { buffer += ch.asInstanceOf[Char]; loop(read) }
	                	else {
	                		
	                	    val nx = read
	                	    
	                	    if (nx < 0) loop(nx)
	                	    else {
	                	        
	                	        nx match {
	                	            case '\\' => { buffer ++= "\\\\"; loop(read) }
	                	            case '"' => { buffer ++= "\""; loop(read) }
	                	            case 't' => { buffer ++= "\t"; loop(read) }
	                	            case 'r' => { buffer ++= "\r"; loop(read) }
	                	            case 'n' => { buffer ++= "\n"; loop(read) }
	                	            case _ => tokVar = Token.Error("invalid escape sequence in string literal", start)
	                	        }
	                	    }
	                	}
	                }
	            }
	            
	            loop(read)
	        }
	        
	        @tailrec def loop {
	            
	            val ch = peek
	            
	            if (ch < 0) {
	                
	                eof = true
	                tokVar = Token.EndOfInput(location)
	                
	            } else {
	                
	                ch match {
	                    
	                    case ' ' | '\n' | '\r' | '\t' => { read; loop }
	                    case '#' => { skipToEol; loop }
	                    case '.' => produce(Token.Period(location)) 
	                    case '=' => produce(Token.Equals(location))
	                    case '{' => produce(Token.OpeningBrace(location))
	                    case '}' => produce(Token.ClosingBrace(location))
	                    case '"' => readLiteral
	                    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => readNumber
	                    case '+' | '-' => readNumber
	                    case wch if isWordStart(wch) => readWord 
	                    case _ => produce(Token.Error("unexpected character", location))
	                }
	            }
	        }
	        
	        loop
	    }
	}
}


object Parser {
    
    import darts.lib.io.Utilities._
    import java.util.regex.Pattern
    import scala.collection.mutable.HashMap
    
    def parse(uri: URI)(implicit config: URLReaderConfiguration): Map[String,String] = 
        parse(uri.toURL)

    def parse(url: URL)(implicit config: URLReaderConfiguration): Map[String,String] = 
        withURLStream(url) { (s,t,u) => parse(s, t) }

    def parse(file: File)(implicit config: URLReaderConfiguration): Map[String,String] = {
        val reader = new InputStreamReader(new BufferedInputStream(new FileInputStream(file)), config.encoding)
        try parse(file.toURI.toASCIIString, reader) finally reader.close
    }
    
    def parse(source: String, stream: InputStream)(implicit config: URLReaderConfiguration): Map[String,String] =
        parse(source, new InputStreamReader(stream, config.encoding))
    
    def parse(string: String): Map[String,String] = 
        parse("<string>", new StringReader(string))
    
    def parse(source: String, reader: Reader): Map[String,String] = {
    		
        val lexer = new Lexer(source, reader)
        val table = new HashMap[String,String]
        
        def unexpected(tok: Token) = throw new IOException(tok.location + ": unexpected token " + tok)
        def lexerror(tok: Token.Error) = throw new IOException(tok.location + ": " + tok.message)
        def badeof(tok: Token.EndOfInput) = throw new IOException(tok.location + ": premature end of input")
        
        def doSub(section: String) {
            doTop(section)
            lexer.token match {
                case Token.ClosingBrace(_) => lexer.advance 
                case tok: Token.EndOfInput => badeof(tok)
                case tok: Token.Error => lexerror(tok)
                case tok => unexpected(tok)
            }
        }
        
        def doAssign(key: String) {
            lexer.token match {
                case Token.Word(wd, _) => { lexer.advance; table(key) = wd }
                case Token.Literal(wd, _) => { lexer.advance; table(key) = wd }
                case tok: Token.EndOfInput => badeof(tok)
                case tok: Token.Error => lexerror(tok)
                case tok => unexpected(tok)
            }
        }
        
        @tailrec def doName(sofar: String) {
            lexer.token match {
                case tok: Token.Error => lexerror(tok)
                case tok: Token.EndOfInput => badeof(tok)
                case Token.OpeningBrace(_) => lexer.advance; doSub(sofar + ".")
                case Token.Equals(_) => lexer.advance; doAssign(sofar)
                case Token.Period(_) => lexer.advance; lexer.token match {
                    case tok: Token.Error => lexerror(tok)
                    case tok: Token.EndOfInput => badeof(tok)
                    case Token.Word(seg, _) => lexer.advance; doName(sofar + "." + seg)
                    case tok => unexpected(tok) 
                }
                case tok => unexpected(tok) 
            }
        }
        
        @tailrec def doTop (prefix: String) {
            lexer.token match {
                case tok: Token.Error => lexerror(tok)
                case Token.Word(sec, _) => lexer.advance; doName(prefix + sec); doTop(prefix)
                case tok => ()
            }
        }
        
        lexer.advance
        doTop("")
        lexer.token match {
            case Token.EndOfInput(_) => table.toMap
            case tok: Token.Error => lexerror(tok)
            case tok => throw new IOException(tok.location + ": trailing garbage")
        }
    }
}

