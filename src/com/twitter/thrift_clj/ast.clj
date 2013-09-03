(ns com.twitter.thrift-clj.ast
  "AST for the Clojure Thrift parser."
  (:refer-clojure :exclude (type))
  (:use [clojure.set :only (map-invert)])
  (:require [clojure.string :as s]))

;; NOTE: Not supporting the slist annotation.
;;
;; TODO: Do we need to delete this bad boy and get rid of the whole
;;keyword thing?

(def type-map
  {"void" :void
   "bool" :boolean
   "byte" :byte
   "i16" :short
   "i32" :integer
   "i64" :long
   "double" :double
   "string" :string
   "binary" :bytes})

(def type->keyword
  (some-fn type-map identity))

(def keyword->type
  (some-fn (map-invert type-map)
           identity))

;; TODO: Figure out what defaults are allowed for booleans, perform
;; validation.

(defn s->bool
  "Converts a string into a bool."
  [s]
  (condp #(% %2) s
    #{"0" "false"} false
    #{"1" "true"} true))

;; ## Protocols

(defprotocol Printable
  (thrift-line [_]
    "Prints out a Thrift-IDL-compatible string description of the
    thrift AST item."))

;; ## AST Types

;; ## Constant Records

(defrecord IntConstant [value]
  Printable
  (thrift-line [_] (str value)))

(defrecord DoubleConstant [value]
  Printable
  (thrift-line [_] (str value)))

(defrecord StringConstant [s]
  Printable
  (thrift-line [_] s))

(defrecord Identifier [s]
  Printable
  (thrift-line [_] s))

(defrecord MapConstant [m]
  Printable
  (thrift-line [_]
    (->> m
         (map (fn [[k v]]
                (format "%s:%s"
                        (thrift-line k)
                        (thrift-line v))))
         (s/join ",")
         (format "{%s}"))))

(defrecord ListConstant [l]
  Printable
  (thrift-line [_]
    (->> (map thrift-line l)
         (s/join ",")
         (format "[%s]"))))

;; TODO: Set Constants?

;; ### Type Records

(defrecord MapType [key-type value-type]
  Printable
  (thrift-line [_]
    (format "map<%s,%s>"
            (thrift-line key-type)
            (thrift-line value-type))))

(defrecord ListType [inner-type]
  Printable
  (thrift-line [_]
    (format "list<%s>" (thrift-line inner-type))))

(defrecord SetType [inner-type]
  Printable
  (thrift-line [_]
    (format "set<%s>" (thrift-line inner-type))))

(defrecord ReferenceType [name]
  Printable
  (thrift-line [_] name))

;; TODO: Add docstrings to everything.

;; TODO: Can we have default values for defrecords? I think there's
;; some library that lets you do these sorts of things. We might be
;; able to do validation on these bad boys with clj-schema.

(def OPTIONAL ::optional)
(def REQUIRED ::required)
(def DEFAULT ::default)

(defn parse-requiredness [s]
  (condp = s
    "optional" OPTIONAL
    "required" REQUIRED
    DEFAULT))

;; ### Other Records

(defrecord Include [filename prefix document]
  Printable
  (thrift-line [_]
    (str "include \"" filename "\"")))

(defrecord CppInclude [filename]
  Printable
  (thrift-line [_]
    (str "cpp_include \"" filename "\"")))

(defrecord Namespace [lang name]
  Printable
  (thrift-line [_]
    (s/join " " ["namespace" lang name])))

;; TODO: Perform some sort of validation on the type name. This parser
;; as it runs now doesn't take any sort of type aliases into
;; account. Some of this stuff only makes sense in the context of the
;; enclosing document, which will need its own high-level validation.

(defrecord EnumValue [name value]
  Printable
  (thrift-line [_] (str name " = " value)))

(defrecord Const [name type value]
  Printable
  (thrift-line [_]
    (let [type-str (keyword->type type)]
      (s/join " " ["const" type-str name "=" value]))))

(defrecord TypeDef [name type]
  Printable
  (thrift-line [_]
    (let [type-str (keyword->type type)]
      (s/join " " ["typedef" type-str name]))))

;; These fields are used for a number of types, from exception fields
;; to function fields.

(defrecord Field [id name type default requiredness])

(defrecord Struct [name fields])

(defrecord ThriftEnum [name fields])

(defrecord ThriftException [name fields])

(defrecord Function [name return-type args oneway? exceptions]
  Printable
  (thrift-line [_]
    (str (s/join " " (into (when oneway? ["oneway"])
                           [return-type name "("]
                           ["("]))
         (s/join ", " (map thrift-line args))
         ")"
         (when exceptions
           (format "throws (%s)"
                   (s/join ", " (map thrift-line exceptions)))))))

(defrecord Service [name parent functions]
  Printable
  (thrift-line [_]
    (let [base  (->> (into ["service" name]
                           (when parent
                             ["extends" parent]))
                     (s/join " "))
          funcs (->> functions
                     (map thrift-line)
                     (s/join ";\n"))]
      (str base " {\n" funcs "\n}"))))

;; Final document wrapper.

(defrecord Document [includes namespaces definitions])
