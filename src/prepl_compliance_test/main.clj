(ns prepl-compliance-test.main
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.net Socket InetSocketAddress]
           [java.io BufferedReader]))

;; These defaults connect to scripts/baseline.sh, start that first.
;; This is mainly intended for REPL based development of this project.
(def ^:dynamic *host* "localhost")
(def ^:dynamic *port* 33945)

(defmacro with-socket [bind & body]
  `(with-open [socket# (Socket.)]
     (.connect socket# (InetSocketAddress. *host* *port*))
     (let [~bind [(io/writer socket#) (BufferedReader. (io/reader socket#))]]
       ~@body)))

(defn write [socket x]
  (.write socket x)
  (.flush socket)
  socket)

(defn read-one [socket]
  (let [!result (future
                  (edn/read-string (.readLine socket)))]
    (loop [attempts 20]
      (cond
        (future-done? !result) @!result
        (> attempts 0) (do
                         (Thread/sleep 5)
                         (recur (dec attempts)))
        :else (throw (Error. "Failed to read one value from socket"))))))

(t/deftest simple-evaluations
  (t/testing "basic evaluation of a form returns the correct data"
    (with-socket [w r]
      (write w "(+ 10 20)")
      (let [ret (read-one r)]
        (t/is (= #{:tag :val :ns :ms :form}
                 (set (keys ret))))
        (t/is (int? (:ms ret)))
        (t/is (>= (:ms ret) 0))
        (t/is (= {:tag :ret
                  :val "30"
                  :ns "user"
                  :form "(+ 10 20)"}
                 (dissoc ret :ms))))))

  (t/testing "multiple forms produce multiple return values"
    (with-socket [w r]
      (write w "(+ 1 0) (+ 1 1) (+ 1 2)")
      (let [results [(read-one r) (read-one r) (read-one r)]]
        (t/is (= ["1" "2" "3"]
                 (mapv :val results)))))))

(t/deftest tap-output
  (t/testing "use of tap> return true and outputs a tap value"
    (with-socket [w r]
      (write w "(tap> :henlo)")
      (let [ret (read-one r)
            tap (read-one r)]
        (t/is (= {:tag :ret, :val "true"}
                 (select-keys ret #{:tag :val})))
        (t/is (= {:tag :tap, :val ":henlo"} tap))))))

(t/deftest stdio
  (t/testing "*out* goes to :out"
    (with-socket [w r]
      (write w "(println :foo)")
      (let [out (read-one r)
            ret (read-one r)]
        (t/is (= {:tag :out, :val ":foo\n"} out))
        (t/is (= {:tag :ret, :val "nil"}
                 (select-keys ret #{:tag :val}))))))

  (t/testing "*err* goes to :err"
    (with-socket [w r]
      (write w "(binding [*out* *err*] (println :foo))")
      (let [out (read-one r)
            ret (read-one r)]
        (t/is (= {:tag :err, :val ":foo\n"} out))
        (t/is (= {:tag :ret, :val "nil"}
                 (select-keys ret #{:tag :val}))))))

  ;; The flush part of this one is debateable.
  ;; ClojureScript prepls flush automatically, Clojure one's don't.
  ;; I think every prepl should flush the output on every eval but maybe that
  ;; could have unintended side effects? For now, you should (flush) on the end
  ;; of each prepl eval to keep *out* working consistently.
  (t/testing "lack of newlines (with flush) is respected"
      (with-socket [w r]
        (write w "(print :foo) (flush)")
        (let [ret-1 (read-one r)
              out (read-one r)
              ret-2 (read-one r)]
          (t/is (= {:tag :ret, :val "nil"}
                   (select-keys ret-1 #{:tag :val})))
          (t/is (= {:tag :out, :val ":foo"} out))
          (t/is (= {:tag :ret, :val "nil"}
                   (select-keys ret-2 #{:tag :val})))))))

(t/deftest reader-conditionals
  (t/testing "reader conditionals can be used"
    (with-socket [w r]
      (write w "#?(:clj :clojure, :cljs :clojurescript)")
      (let [{:keys [ret]} (read-one r)]
        (t/is (or (= ":clojure" ret) (= ":clojurescript" ret)))))))

(t/deftest exceptions
  (t/testing "exceptions are run through (Throwable|Error)->map before returning"
    (with-socket [w r]
      (write w "(thing-that-doesnt-exist)")
      (let [ret (read-one r)]
        (t/is (re-matches #"\{:via \[\{:type .*, :phase :compile-syntax-check\}" (:val ret))))))

  (t/testing "exceptions are marked with :exception true"
    (with-socket [w r]
      (write w "(thing-that-doesnt-exist)")
      (let [ret (read-one r)]
        (t/is (= true (:exception ret)))))))

(defn -main [host port]
  (println "Hello, intrepid tool author!")
  (println "These tests are based upon my blog post: https://oli.me.uk/clojure-prepl-for-tool-authors/")
  (println (str "We'll be testing the prepl server hosted at " host ":" port "."))
  (newline)

  (binding [*host* host
            *port* (Integer/parseInt port)]
    (t/run-tests)))
