(ns com.twitter.thrift-clj.ast-test
  (:use com.twitter.thrift-clj.ast :reload)
  (:use midje.sweet))

;; ## Helper Functions

(defn thrift-prints
  "Accepts a string and returns a checker that checks that its input,
  fed into the thrift-line function, matches the supplied string."
  [to-str]
  (chatty-checker [item]
                  (= (thrift-line item)
                     to-str)))

(fact
  "consts print properly as thrift."
  (->Const "FACE" :integer 123) => (thrift-prints "const i32 FACE = 123")
  (->Const "FACE" "FireValue" 123) => (thrift-prints "const FireValue FACE = 123")

  "typedefs print properly as thrift."
  (->TypeDef "dingle" :long) => (thrift-prints "typedef i64 dingle"))
