(ns com.twitter.thrift-clj.insta
  (:require [instaparse.core :as insta]
            [clojure.pprint :refer (pprint)]))

(def instagram
  (insta/parser
   "
Document        ::= <ws> Header* Definition*
SlashComment    ::= <'//'> #'[^\n]'*
PoundComment    ::= <'#'> #'[^\n]'*
BlockComment    ::= '/*' #'(?s).'* '*/'
Comment         ::= SlashComment | PoundComment | BlockComment | <ws>
ws              ::= <[#'\\s*']> | (Comment <ws>)
Header          ::=  (Include | CppInclude | Namespace)
Include         ::=  <'include'> <ws> Literal <ws>
CppInclude      ::=  <'cpp_include'> <ws> Literal <ws>
Namespace       ::=  (( <'namespace'> <ws> ( NamespaceScope Identifier ) |
                                        ( <'smalltalk.category'> <ws> STIdentifier ) |
                                        ( <'smalltalk.prefix'> <ws> Identifier ) ) |
                          ( <'php_namespace'> <ws> Literal ) |
                          ( <'xsd_namespace'> <ws> Literal )) <ws>
NamespaceScope  ::=  ('*' | 'cpp' | 'java' | 'py' | 'perl' | 'rb' | 'cocoa' | 'csharp' | 'clojure') <ws>
Definition      ::=  (Const | Typedef | Enum | Senum | Struct | Exception | Service) <ws>
Const           ::=  <'const'> <ws> FieldType Identifier <'='> <ws> ConstValue ListSeparator?
Typedef         ::=  <'typedef'> <ws> DefinitionType Identifier
Enum            ::=  <'enum'> <ws> Identifier <'{'> <ws> (EnumEntry | DefaultEntry)* <ws> <'}'>
EnumEntry       ::=  Identifier <ListSeparator?>
DefaultEntry    ::=  Identifier DefaultValue <ListSeparator?>
DefaultValue    ::=  <'='> <ws> IntConstant <ws>
Senum           ::=  <'senum'> <ws> Identifier <'{'> (Literal ListSeparator?)* <'}'>
Struct          ::=  <'struct'> <ws> Identifier <ws> ('xsd_all'? | <ws>) <'{'> (<ws> Field <ws>)* <'}'>
Exception       ::=  <'exception'> <ws> Identifier <'{'> (<ws> Field)* <'}'>
Service         ::=  <'service'> <ws> Identifier ( <'extends'> <ws> Identifier )? <'{'> (<ws> Function <ws>)* <'}'>
Field           ::=  FieldID <ws> FieldReq? <ws> FieldType <ws> Identifier <ws> ('=' <ws> ConstValue)? <ws> XsdFieldOptions <ws> <ListSeparator>? <ws>
FieldID         ::=  (IntConstant <ws> <':'>) <ws>
FieldReq        ::=  ('required' | 'optional') <ws>
XsdFieldOptions ::=  'xsd_optional'? 'xsd_nillable'? XsdAttrs?
XsdAttrs        ::=  'xsd_attrs' '{' Field* '}'
Function        ::=  <('oneway' ws)?> FunctionType Identifier '(' Field* ')' Throws? ListSeparator?
FunctionType    ::=  FieldType | 'void'
Throws          ::=  'throws' '(' Field* ')'
FieldType       ::=  Identifier | BaseType | ContainerType
DefinitionType  ::=  BaseType | ContainerType
BaseType        ::=  ('bool' | 'byte' | 'i16' | 'i32' | 'i64' | 'double' | 'string' | 'binary' | 'slist') <ws>
ContainerType   ::=  MapType | SetType | ListType
MapType         ::=  <'map'> <ws> CppType? <'<'> <ws> FieldType <','> FieldType <'>'> <ws>
SetType         ::=  <'set'>  <ws> CppType? <'<'> FieldType <'>'> <ws>
ListType        ::=  <'list'> <ws> <'<'> FieldType <'>'> CppType? <ws>
CppType         ::=  <'cpp_type'> <ws> Literal
ConstValue      ::=  (IntConstant | DoubleConstant | Literal | Identifier | ConstList | ConstMap)
IntConstant     ::=  ('+' | '-')? Digit+ <ws>
DoubleConstant  ::=  ('+' | '-')? Digit* ('.' Digit+)? ( ('E' | 'e') IntConstant )? <ws>
ConstList       ::=  <'['> (ConstValue ListSeparator?)* <']'> <ws>
ConstMap        ::=  <'{'> (ConstValue <':'> ConstValue <ListSeparator>?)* <'}'> <ws>
Literal         ::=  ((<'\\\"'> #'[^\"]+'* <'\\\"'>) | (<\"'\"> #'[^\\']+'* <\"'\">)) <ws>
Identifier    ::=  (( Letter | '_' ) ( Letter | Digit | '.' | '_' )*) <ws>
STIdentifier  ::=  ( Letter | '_' ) ( Letter | Digit | '.' | '_' | '-' )* <ws>
ListSeparator ::=  (',' | ';') <ws>
<Letter>      ::=  #'[a-zA-Z]'
<Digit>       ::=  #'[0-9]'"))
