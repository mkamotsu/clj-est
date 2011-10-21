(ns btonic.core
  (:require [com.twinql.clojure.http :as http])
  (:require [clojure.contrib.lazy-xml :as xml])
  (:gen-class))

(def *url* "http://btonic.est.co.jp/NetDic/NetDicV09.asmx")

(defn url-encode [^String url]
  (java.net.URLEncoder/encode url))

(defn url-for-search-dict-item-lite [dic word scope match merge prof page-size page-index]
  (format
   "%s/SearchDicItemLite?Dic=%s&Word=%s&Scope=%s&Match=%s&Merge=%s&Prof=%s&PageSize=%d&PageIndex=%d"
   *url*
   (case dic
     (:ejdict) "EJdict"
     (:editje) "EditJE"
     (:wpedia) "wedia")
   (url-encode word)
   (case scope
     (:headword) "HEADWORD"
     (:anyware) "ANYWHARE")
   (case match
     (:startwith) "STARTWITH"
     (:endwith) "ENDWITH"
     (:contain) "CONTAIN"
     (:exact) "EXACT")
   (case merge
     (:and) "AND"
     (:or) "OR") prof page-size page-index))

(defn url-for-get-dic-item-lite
  ([dic item] (url-for-get-dic-item-lite dic item "" "XHTML"))
  ([dic item loc prof]
     (format
      "%s/GetDicItemLite?Dic=%s&Item=%s&Loc=%s&Prof=%s"
      *url*
      (case dic
        (:ejdict) "EJdict"
        (:editje) "EditJE"
        (:wpedia) "wpedia")
      item loc prof)))

(defn get-bytes [^String string]
  (.getBytes string "utf-8"))

(defn search-dict-item-lite
  ([dic word page-size page-index] (search-dict-item-lite dic word :headword :exact :and "XHTML" page-size page-index))
  ([dic word scope match merge prof page-size page-index]
     (let [response (http/get (url-for-search-dict-item-lite
                               dic word scope match merge prof page-size page-index) :as :string)
           s (java.io.ByteArrayInputStream. (get-bytes (:content response)))]
       (let [results (for [element (xml/parse-seq s) :when (= :characters (:type element))]
                       (:str element))]
         {:total-hit-count (Integer/parseInt (first results))
          :items (partition 2 (drop 2 results))}))))

(defn get-dic-item-lite
  ([dic item] (get-dic-item-lite dic item "" "XHTML"))
  ([dic item loc prof]
     (let [response (http/get (url-for-get-dic-item-lite dic item loc prof) :as :string)
           s (java.io.ByteArrayInputStream. (get-bytes (:content response)))]
       (for [element (xml/parse-seq s) :when (= :characters (:type element))]
         (:str element)))))

(defn ejdict [word]
  (let [items (:items (search-dict-item-lite :ejdict word 1 0))]
    (if (empty? items) nil
        (get-dic-item-lite :ejdict (first (first items))))))

(defn -main [& args]
  (loop []
    (print "> ")
    (flush)
    (let [input (str (read))
          response (ejdict input)]
      (println input)
      (println (apply str
                      (replace {\tab \newline}
                               (second response)))))
    (recur)))
