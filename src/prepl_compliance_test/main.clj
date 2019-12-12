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
  (edn/read-string (.readLine socket)))

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
                 (dissoc ret :ms)))))))

(t/deftest tap-output
  (t/testing "use of tap> return true and outputs a tap value"
    (with-socket [w r]
      (write w "(tap> :henlo)")
      (let [ret (read-one r)
            tap (read-one r)]
        (t/is (= {:tag :ret, :val "true"}
                 (select-keys ret #{:tag :val})))
        (t/is (= {:tag :tap, :val ":henlo"} tap))))))

(defn -main [host port]
  (println "Hello, intrepid tool author!")
  (println "These tests are based upon my blog post: https://oli.me.uk/clojure-prepl-for-tool-authors/")
  (println (str "We'll be testing the prepl server hosted at " host ":" port "."))
  (newline)

  (binding [*host* host
            *port* (Integer/parseInt port)]
    (t/run-tests)))
