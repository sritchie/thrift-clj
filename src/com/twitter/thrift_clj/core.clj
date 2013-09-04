(ns com.twitter.thrift-clj.core
  (:refer-clojure :exclude (namespace type))
  (:use clojure.algo.monads
        [clojure.pprint :only (pprint)]
        [clojure.string :only (lower-case)]
        com.twitter.thrift-clj.ast
        com.twitter.thrift-clj.mparser
        com.twitter.thrift-clj.util)
  (:require [instaparse.core :as insta]))

;; ## Thrift-Specific Parsers

(with-monad parser-m
  (let [la "abcdefghijklmnopqrstuvwxyz"
        ua (set (.toUpperCase la))
        la (set la)
        letters (into la ua)
        signs (set "+-")
        sp (set " \t\n\r")
        numbers (set "1234567890")
        characters (reduce into [letters numbers #{\_}])
        list-sep (set ",;")]

    (def ^{:doc "Match + or -."}
      sign
      (one-of signs))

    (def ^{:doc "Match any whitespace character."}
      whitespace-char
      (one-of sp))

    (def ^{:doc "Match any list separator."}
      match-sep
      (one-of list-sep))

    (def ^{:doc "Match anything except the list separators and whitespace."}
      non-whitespace-char
      (not-one-of (into list-sep sp)))

    (def ^{:doc "Match any upper-case letter."}
      upper-case-letter
      (one-of ua))

    (def ^{:doc "Match any lower-case letter."}
      lower-case-letter
      (one-of la))

    (def ^{:doc "Match any letter from the alphabet, in both lower and
      upper case."}
      letter
      (one-of letters))

    (def ^{:doc "Match any numeric character."}
      number
      (one-of numbers))

    (def ^{:doc "Match any number or letter."}
      character
      (one-of characters))

    (def ^{:doc "Match any valid namespace character."}
      ns-character
      (one-of (conj characters \.))))

  (defn all-between
    "Matches any character with the supplied starting and ending
delimiters. If only a starting string is supplied, the end defaults to
the reverse of this string. /* and */, for example.)"
    ([start] (all-between start (reverse start)))
    ([start end]
       (domonad [_ (match-string start)
                 text (skip-until-items end)
                 _ (match-string end)]
                (to-str text))))

  (defn comment-line
    "Matches a line comment beginning with the supplied starting
    string."
    [starting-str]
    (domonad [_    (match-string starting-str)
              text (none-or-more (not-one \newline))]
             (to-str text)))

  (def c-comment-block (all-between "/*" "*/"))

  (def ^{:doc "Matches a comment line beginning with \"//\"."}
    slash-comment
    (comment-line "//"))

  (def  ^{:doc "Matches a comment line beginning with \"#\"."}
    pound-comment
    (comment-line "#"))

  (def whitespace
    (match-one (skip-one-or-more whitespace-char)
               c-comment-block
               pound-comment
               slash-comment))

  (defn lexeme
    "Augments the supplied parser to skip all trailing whitespace."
    [parser]
    (domonad [res parser
              _ (skip-none-or-more whitespace)]
             res))

  (def ^{:doc "Match any thrift list separator."}
    list-separator
    (lexeme match-sep))

  (defn symb
    "Match the supplied symbol. Skips trailing whitespace."
    [name]
    (lexeme (match-string name)))

  (defn some-symb
    "Match one of the supplied symbols. Skips trailing whitespace."
    [& names]
    (lexeme (apply match-one (map match-string names))))

  (def ^{:doc "Parser that slurps up any non-whitespace string."}
    any-string
    (domonad [chars (lexeme (one-or-more non-whitespace-char))]
             (to-str chars)))

  (def ^{:doc "Matches the next token in the stream."}
    next-token
    (domonad [chars (lexeme (one-or-more character))]
             (to-str chars)))

  (def ^{:doc "Matches the next namespaced token in the stream."}
    ns-token
    (domonad [chars (lexeme (one-or-more ns-character))]
             (to-str chars)))

  ;; ## Header Parsers
  ;;
  ;; include, cpp_include, namespace

  ;; TODO: Determine allowed characters. I know we need to include
  ;; numbers as well.

  (def ^{:doc "Parse a single namespace out of the supplied language's
    name declaration."}
    namespace
    (domonad [_       (symb "namespace")
              lang    next-token
              ns-name ns-token]
             (->Namespace lang ns-name)))

  ;; TODO: Validation. What if we have multiple declarations for the
  ;; same language?
  (def ^{:doc "A map of all namespaces."}
    namespaces
    (one-or-more namespace))

  (defn between
    "Returns parser's match between open and close."
    [open parser close]
    (domonad [_ open
              res parser
              _ close]
             res))

  (defn split
    "Returns two successive matches of the same parser split by the
    sep parser."
    [parser sep]
    (domonad [a parser
              _ sep
              b parser]
             [a b]))

  (defn quoted
    "Returns parser's match between two quotes."
    [p]
    (lexeme (between (one \") p (one \"))))

  (defn braced
    "Returns parser's match between two angle braces."
    [p]
    (between (symb "<") p (symb ">")))

  (defn curly-braced
    "Returns parser's match between two curly braces."
    [p]
    (between (symb "{") p (symb "}")))

  (defn bracketed
    "Returns parser's match between two square brackets."
    [p]
    (between (symb "[") p (symb "]")))

  (defn parens
    "Returns parser's match between two parens."
    [p]
    (between (symb "(") p (symb ")")))

  (defn match-include
    "Parse an included file form, signaled by the supplied prefix. By
default, include-type will be the keyword form of the supplied
prefix."
    ([constructor-fn prefix]
       (domonad [_ (symb prefix)
                 relative-path (quoted (one-or-more ns-character))
                 :let [s (to-str relative-path)]]
                (constructor-fn s)
                )))

  ;; TODO: Replace the "nil" with a call to the full parser.

  ;; Declare the thrift-parser to allow the include statement to grab
  ;; the proper document recursively.
  (declare thrift-parser)

  (def ^{:doc "Parse a single include statement."}
    include
    (match-include #(->Include % (strip-extension %) nil)
                   "include"))

  (def ^{:doc "Parse a single cpp_include statement."}
    cpp-include
    (match-include ->CppInclude "cpp_include"))

  (def ^{:doc "Sequence of all thrift include maps."}
    includes
    (none-or-more (match-one include cpp-include)))

  ;; ## Constant parsers
  ;;
  ;; TODO: parse all numeric constants into their proper types and
  ;;formats.

  ;; ## Type Parsers
  ;;
  ;; const, typedef, enum, senum, struct, exception, service (with
  ;;extends?)

  (defn match-type
    "Supply a constructor-fn of two args, a name (like struct), and a
    final transformer. `constructor-fn` receives the parsed name and
    the result of the supplied parser."
    [constructor-fn type-name type-parser]
    (domonad [_    (symb type-name)
              sym  next-token
              body (curly-braced type-parser)]
             (constructor-fn sym body)))

  (def field-id
    (domonad [field-id (one-or-more number)
              _ (lexeme (one \:))]
             (Integer/parseInt (to-str field-id))))

  ;; ## Constant Parsers

  (def signed-number
    (domonad [sign (optional sign)
              :let [sign (if (= sign \-) "-" "")]
              pieces (one-or-more number)]
             (apply str sign pieces)))

  (def exponent
    (match-all (one-of (set "eE"))
               signed-number))

  (def match-long
    (lexeme
     (domonad [num signed-number]
              (->IntConstant (Long/parseLong num)))))

  (def match-double
    (lexeme
     (domonad [num signed-number
               dec (match-all (one \.)
                              (one-or-more number)
                              (optional exponent))]
              (->DoubleConstant (Double/parseDouble (apply str num dec))))))

  (def number-constant
    (match-one match-double
               match-long))

  (def bool-constant
    (m-bind (some-symb "true" "false" "0" "1")
            (comp m-result s->bool)))

  (def string-constant
    (lexeme
     (domonad [s (match-one (all-between "'")
                            (all-between "\""))]
              (->StringConstant s))))

  (declare constant)

  (def ^{:doc "Parses a list constant of the form "}
    list-constant
    (domonad [items (bracketed
                     (repsep constant list-separator))]
             (->ListConstant items)))

  (def map-entry
    (domonad [[k-const v-const] (split constant (symb ":"))]
             {k-const v-const}))

  (def map-constant
    (domonad [entries (curly-braced
                       (repsep map-entry list-separator))]
             (->MapConstant (reduce into {} entries))))

  (def identifier
    (m-bind ns-token
            (comp m-result ->Identifier)))

  (def constant
    (match-one number-constant
               bool-constant
               string-constant
               list-constant
               map-constant
               identifier))

  (def ^{:doc "Parses a thrift default value."}
    default-val
    (domonad [_ (symb "=")
              const constant]
             const))

  ;; ## Type Parsers

  (def base-type
    (some-symb "bool" "byte" "i16" "i32" "i64"
               "double" "string" "binary"))

  (declare field-type)

  ;; Next three match the container types.

  (def map-type
    (let [kv-types (split field-type (symb ","))]
      (domonad [_ (symb "map")
                [key-type val-type] (braced kv-types)]
               (->MapType key-type val-type))))

  (def list-type
    (domonad [_ (symb "list")
              inner-type (braced field-type)]
             (->ListType inner-type)))

  (def set-type
    (domonad [_ (symb "set")
              inner-type (braced field-type)]
             (->SetType inner-type)))

  (def container-type
    (match-one map-type set-type list-type))

  (def reference-type
    (m-bind next-token
            (comp m-result ->ReferenceType)))

  (def field-type
    (match-one base-type
               container-type
               reference-type))

  ;; ### Structs and Exceptions

  (def field-requiredness
    (domonad [req (some-symb "optional" "required")]
             (parse-requiredness req)))

  (def ^{:doc "Parses a thrift field."}
    field
    (domonad [id           (lexeme field-id)
              requiredness (optional field-requiredness)
              type         field-type
              name         next-token
              default      (optional default-val)
              _            (optional list-separator)]
             (->Field id name type default (or requiredness
                                               DEFAULT))))

  (def match-struct
    (match-type ->Struct "struct" (one-or-more field)))

  (def match-exception
    (match-type ->ThriftException "exception" (one-or-more field)))

  ;; ### Enum

  (def ^{:doc "enum field parser"}
    enum-field
    (domonad [name  next-token
              [val] (match-all (optional default-val)
                               (optional list-separator))]
             (->EnumValue name val)))

  ;; TODO: Make sure that no enum values are repeated.
  (def ^{:doc "Handles logic for assigning default values to enums."}
    enum-fields
    (letfn [(next-id [acc]
              (->IntConstant
               (if-let [prev (-> acc peek :value :value)]
                 (inc prev)
                 0)))]
      (domonad [fields (one-or-more enum-field)]
               (reduce (fn [acc item]
                         (conj acc
                               (if (:value item)
                                 item
                                 (assoc item :value (next-id acc)))))
                       []
                       fields))))

  (def match-enum
    (match-type ->ThriftEnum "enum" enum-fields))

  ;; ### Typedefs and Constants

  (def match-typedef
    (domonad [_     (symb "typedef")
              type  field-type
              alias next-token
              _     (optional list-separator)]
             (->TypeDef alias type)))

  (def match-const
    (domonad [_   (symb "const")
              type field-type
              name next-token
              val  default-val
              _    (optional list-separator)]
             (->Const name type val)))

  ;; ### Services

  (def func-field
    (domonad [id   field-id
              type field-type
              name next-token
              _    (optional list-separator)]
             ;; TODO: Can we have defaults or required values in these
             ;; fields?
             (->Field id name type nil REQUIRED)))

  (def function-type
    (match-one (symb "void") field-type))

  (def function
    (domonad [oneway?          (optional (symb "oneway"))
              return-type      function-type
              func-name        next-token
              args             (parens (none-or-more func-field))
              [_ & exceptions] (optional
                                (match-all (symb "throws")
                                           (parens (none-or-more func-field))))
              _                (optional list-separator)]
             (->Function func-name
                         return-type
                         args
                         (boolean oneway?)
                         (when exceptions
                           (into [] exceptions)))))

  (def match-service
    (domonad [_   (symb "service")
              sym  next-token
              [_ extends] (optional (match-all (symb "extends")
                                               ns-token))
              body (curly-braced (one-or-more function))
              _    (optional list-separator)]
             (->Service sym extends body)))

  ;; ### Final Matchers

  (def ^{:doc "Matches a non-service thrift definition."}
    definition
    (match-one match-struct
               match-exception
               match-enum
               match-typedef
               match-const
               match-service))

  (def definitions (none-or-more definition))

  ;; TODO: Add parseFile method.

  (def thrift-parser
    "top-level parser."
    (domonad [_        (skip-none-or-more whitespace)
              incls    includes
              ns-decls namespaces
              defs     definitions
              _ eof]
             (->Document incls ns-decls defs))))

(def p
  (insta/parser "<edges> : line+
line : <whitespace> letter <'->'> letter <whitespace> number <whitespace>
letter : #'[A-Z]'
number : #'[0-9]+'
whitespace : #'\\s*'
"))

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
Enum            ::=  <'enum'> <ws> Identifier <'{'> ws (Identifier ws ('=' ws IntConstant)? ListSeparator?)* <'}'>
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
Identifier      ::=  ( Letter | '_' ) ( Letter | Digit | '.' | '_' )* <ws>
STIdentifier    ::=  ( Letter | '_' ) ( Letter | Digit | '.' | '_' | '-' )* <ws>
ListSeparator ::=  (',' | ';') <ws>
<Letter>          ::=  #'[a-zA-Z]'
<Digit>           ::=  #'[0-9]'"))


(instagram
 "/**
* Just some comment nastiness. // nested line.
*
*
*/

// And more comments by me.
# Bullshit.

// include \"DOESNTEXIST.thrift\"
 include \"face.thrift\"
 cpp_include \"boggle.thrift\"

namespace clojure forma.schema
namespace rb forma.schema

/**
 * Thrift lets you do typedefs to get pretty names for your types. Standard
 * C style here.
 */
typedef i32 MyInteger
/**
 * Thrift also lets you define constants for use across languages. Complex
 * types and structs are specified using JSON notation.
 */
const i32 INT32CONSTANT = 9853

struct /* random whitespace */ FireValue {
  1: i32 temp330;
  2: i32 conf50;
  3: i32 bothPreds;
  4: i32 count;
}

/**
 * Structs can also be exceptions, if they are nasty.
 */
exception InvalidOperation {
  1: i32 what,
  2: string why
}

service Calculator extends face.SharedService {
  /**
   * A method definition looks like C code. It has a return type, arguments,
   * and optionally a list of exceptions that it may throw. Note that argument
   * lists and exception lists are specified using the exact same syntax as
   * field lists in struct or exception definitions.  NOTE: Overloading of
   * methods is not supported; each method requires a unique name.
   */

   void ping(),

   i32 add(1:i32 num1, 2:i32 num2),

   i32 calculate(1:i32 logid, 2:FireValue fv) throws (1:InvalidOperation ouch),

   /**
    * This method has an oneway modifier. That means the client only makes
    * a request and does not listen for any response at all. Oneway methods
    * must be void.
    *
    * The server may execute async invocations of the same client in parallel/
    * out of order.
    */
   oneway void zip(),
}

struct FormaValue {
  1: FireValue fireValue;
  2: double shortDrop;
  3: double longDrop;
  4: double tStat;
  5: optional double paramBreak;
}
")
