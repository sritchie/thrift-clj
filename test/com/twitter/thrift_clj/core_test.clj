(ns com.twitter.thrift-clj.core-test
  (:refer-clojure :exclude (namespace))
  (:use com.twitter.thrift-clj.core :reload)
  (:use com.twitter.thrift-clj.ast
        [midje.sweet :exclude (one-of)]
        [midje.checkers.extended-equality :only (extended-=)])
  (:import [com.twitter.thrift_clj.ast Field Function]))

;; ## Extra Checkers

(defn parses-to
  "Generates a checker that accepts a result form and applies it
  against the supplied [result state] combination."
  [result]
  (fn [[forms state]]
    (if (fn? result)
      (result forms)
      (extended-= result forms))))

(defn contains-p
  "Generates a checker that accepts a result form and applies it
  against the supplied [result state] combination."
  [result]
  (fn [[forms state]]
    ((contains result) forms)))

(defn is-type
  "Returns a checking function that returns true if the supplied
  instance is an instance of the original supplied klass, false
  otherwise. For example:

  (fact 123 => (is-type Long))
  ;;=> true "
  [klass]
  (fn [item]
    (instance? klass item)))

(def service-string
  "service Calculator extends face.SharedService {

  /**
   * A method definition looks like C code. It has a return type, arguments,
   * and optionally a list of exceptions that it may throw. Note that argument
   * lists and exception lists are specified using the exact same syntax as
   * field lists in struct or exception definitions.  NOTE: Overloading of
   * methods is not supported; each method requires a unique name.
   */

   void ping(),

   i32 add(1:i32 num1, 2:i32 num2),

   i32 calculate(1:i32 logid, 2:Work w) throws (1:InvalidOperation ouch),

   /**
    * This method has an oneway modifier. That means the client only makes
    * a request and does not listen for any response at all. Oneway methods
    * must be void.
    *
    * The server may execute async invocations of the same client in parallel/
    * out of order.
    */
   oneway void zip(),
}")

(fact
  "Improper field decls return nil."
  (func-field "hi!") => (parses-to nil?)

  "Fields are properly parsed."
  (func-field "1:i32 num1") => (contains-p {:name "num1"
                                            :id 1
                                            :type "i32"})

  "Field parsing ignores spaces after field id."
  (func-field "32: i64 cakeFace;") => (contains-p {:name "cakeFace"
                                                   :id 32
                                                   :type "i64"}))

(fact "Function correctly parses two fields."
  (function "i32 add(1:i32 num1, 2:i64 num2),")
  => (contains-p {:name "add"
                  :return-type "i32"
                  :args (n-of (is-type Field) 2)
                  :oneway? false}))

(fact "Parsed service has proper names and four functions."
  (match-service service-string) =>
  (contains-p {:name "Calculator"
               :parent "face.SharedService"
               :functions (n-of (is-type Function) 4)}))

(fact "const parsing."
  (match-const "const i32 INT32CONSTANT = 9853") => (parses-to "face"))

;; ## Examples

(def forma
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

enum Operation {
  ADD,
  SUBTRACT = 2,
  MULTIPLY // should have a value of 3
  DIVIDE = 4
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

(fact "Brittle test of parsing an entire file."
  (thrift-parser forma)
  => (parses-to
      (->Document
       [(->Include "face.thrift" "face" nil)
        (->CppInclude "boggle.thrift")]
       [(->Namespace "clojure" "forma.schema")
        (->Namespace "rb" "forma.schema")]
       [(->TypeDef "MyInteger" "i32")
        (->Const "INT32CONSTANT" "i32" (->IntConstant 9853))
        (->Struct "FireValue"
                  [(->Field 1 "temp330" "i32" nil DEFAULT)
                   (->Field 2 "conf50" "i32" nil DEFAULT)
                   (->Field 3 "bothPreds" "i32" nil DEFAULT)
                   (->Field 4 "count" "i32" nil DEFAULT)])
        (->ThriftEnum "Operation"
                      [(->EnumValue "ADD" (->IntConstant 0))
                       (->EnumValue "SUBTRACT" (->IntConstant 2))
                       (->EnumValue "MULTIPLY" (->IntConstant 3))
                       (->EnumValue "DIVIDE" (->IntConstant 4))])
        (->ThriftException "InvalidOperation"
                           [(->Field 1 "what" "i32" nil DEFAULT)
                            (->Field 2 "why" "string" nil DEFAULT)])
        (->Service "Calculator"
                   "face.SharedService"
                   [(->Function "ping" "void" nil false nil)
                    (->Function "add" "i32"
                                [(->Field 1 "num1" "i32" nil REQUIRED)
                                 (->Field 2 "num2" "i32" nil REQUIRED)]
                                false
                                nil)
                    (->Function
                     "calculate" "i32"
                     [(->Field 1 "logid" "i32" nil REQUIRED)
                      (->Field 2 "fv" (->ReferenceType "FireValue") nil REQUIRED)]
                     false
                     [(->Field 1 "ouch"
                               (->ReferenceType "InvalidOperation") nil REQUIRED)])
                    (->Function "zip" "void" nil true nil)])
        (->Struct "FormaValue"
                  [(->Field 1 "fireValue"
                            (->ReferenceType "FireValue") nil DEFAULT)
                   (->Field 2 "shortDrop" "double" nil DEFAULT)
                   (->Field 3 "longDrop" "double" nil DEFAULT)
                   (->Field 4 "tStat" "double" nil DEFAULT)
                   (->Field 5 "paramBreak" "double" nil OPTIONAL)])])))
