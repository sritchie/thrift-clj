(ns com.twitter.thrift-clj.mparser
  (:use clojure.algo.monads
        com.twitter.thrift-clj.util))

;; # Monadic parser for the [Thrift IDL](http://thrift.apache.org/docs/idl/).
;;
;;  Huge thanks to http://albert.rierol.net/clojure-monads.html and
;;  https://gist.github.com/3672948.
;;
;; ## primitives: the basic building blocks

(def parser-m (state-t maybe-m))

(with-monad parser-m
  (defn parse
    "run-parser takes a top level parser and a string or seq'able to
    run the parser on"
    [parser input]
    (parser (lazy-seq input)))

  (def ^{:doc "Gets the next item from the input and returns it,
    while also updating the state to be the sequence starting
    at the next element."}
    get-one
    (domonad [input (fetch-state)
              _ (set-state (next input))]
             (first input)))

  ;; TODO: What if there aren't enough?
  (defn get-n
    "Gets the next n items from the input and returns it,
    while also updating the state to be the sequence starting
    at the next element."
    [n]
    (domonad [input (fetch-state)
              _ (set-state (drop n input))]
             (take n input)))

  (def ^{:doc "Returns nil to signal that the end of the sequence has
    been reached, otherwise fails in monadic way to indicate that the
    end can't be found here, and therefore the parsing has to
    backtrack to try something else or fail altogether."}
    eof
    (domonad [remaining (fetch-state)
              :when (empty? remaining)]
             nil))

  ;; ## Basic, generic parsers, built on top of the primitives

  (defn matching
    "The most basic matching parser. Tests the next item
    in the sequence against the predicate provided. If true,
    returns the item, otherwise fails."
    [pred]
    (domonad [one get-one
              :when (pred one)]
             one))

  (defn one
    "Next element matches x exactly. What this function does
    is to invoke the matching function with a new anonymous function
    that uses the equality operator to compare x with the next element."
    [x]
    (matching #(= x %)))

  (defn not-one
    "Next element will not match x. Enforces that the element to match
    cannot be nil, which would get confused with the end of sequence."
    [x]
    (matching (fn [y]
                (and (not= x y)
                     (not= nil y)))))

  (defn one-of
    "Matches any one item in the provided set. This function invokes
    the matching function with the provided set as argument; the set
    works as a predicate, because sets in clojure are functions of
    their elements, that is, sets are also functions, which return
    the stored element when given an equal element as argument."
    [s]
    (matching s))

  (defn not-one-of
    "Matches any one item not in the provided set, and returns it.
    To make this work the set cannot be used directly as predicate
    of the matching function, but instead an anonymous function is
    constructed that returns true when the element is not part of the set.
    Enforces that the element to match cannot be nil, which would
    get confused with the end of the sequence."
    [s]
    (matching (fn [y]
                (and (nil? (s y))
                     (not= nil y)))))

  ;; ## Combinators

  (defn optional
    "Return a parser that makes the given parser optional.
     This is accomplished by using m-plus to combine two monadic
     functions: the one provided (the parser) and also (m-result nil)
     which signals a void, but valid monadic return value. If the
     parser doesn't match, then (m-result nil) is returned, signaling
     that the state machine did not advance to the next step."
    [parser]
    (m-plus parser (m-result nil)))

  (defn one-or-more
    "Matches the same parser one or more times until it fails,
     then it returns a sequence of the matched results. Given
     its recursive call, this function can overflow the stack
     when positively matching sequences longer than the possible
     stack depth.
     First the parser is used to match the first item in the sequence,
     and if successful, an optional recursive call is done to match
     further consecutive items. Finally a flattened sequence is returned
     with all matched items in order.
     Given that the parser-m is a modification of the maybe-m, the
     second operation will not be attempted unless the first operation
     succeeded."
    [parser]
    (domonad [r parser
              rs (optional (one-or-more parser))]
             (if rs
               (into [r] (flatten rs))
               [r])))

  (defn none-or-more
    "Matches the same parser zero or more times until it fails,
     then it returns a sequence of the matched results."
    [parser]
    (optional (one-or-more parser)))

  (defn skip-one-or-more
    "Matches the same parser one of more times until it fails,
     then it returns true. Or nil if it doesn't match at least once.
     Given its recursivity this function can overflow the stack.
     This function works like one-or-more, except that it doesn't
     bind neither return the matched values."
    [parser]
    (domonad [_ parser
              _ (optional (skip-one-or-more parser))]
             true))

  (defn skip-none-or-more
    "Matches the same parser zero or more times until it fails,
     then returns true."
    [parser]
    (optional (skip-one-or-more parser)))

  ;; ## Parser combinators

  (defn match-one
    "Match at least one of the given parsers, as evaluated in order,
     or else fail. What this function does is to return a nested
     set of functions of the state using m-plus. When executed,
     when one matches the chain stops and the current matched item
     or sequence of items is returned, according to the parser."
    [& parsers]
    (reduce m-plus parsers))

  (defn match-all
    "Match all given parsers, else fail. Returns a flattened sequence
     with all results. This is accomplished by generating a sequence of
     nested functions which, when invoked with the state as argument,
     thread the state altering it as each is invoked, while the results
     are accumulated in a sequence."
    [& parsers]
    (m-bind (m-seq parsers)
            (comp m-result flatten)))

  (defn match-string
    "Match the supplied string, else fail."
    [s]
    (domonad [res (apply match-all (map one s))]
             (to-str res)))

  (defn repsep
    "Matches the same parser one or more times until it fails,
    skipping intermediate matches of the sep parser. Returns all
    matches of parser as a sequence."
    [parser sep]
    (domonad [r  parser
              rs (optional (match-all sep (repsep parser sep)))]
             (if rs
               (into [r] (flatten (rest rs)))
               [r])))

  (defn skip-until-items
    "Returns each item in the sequence up until a match of the
    provided sequence. If the provided sequence is never found,
    fails."
    [items]
    (domonad [coll (fetch-state)
              :let [items   (seq items)
                    to-drop (->> coll
                                 (partition-all (count items) 1)
                                 (take-while #(not= % items))
                                 (count))]
              :when (< to-drop (count coll))
              predecessors (get-n to-drop)]
             predecessors)))
