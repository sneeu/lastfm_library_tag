(ns lastfm_library_tag.core
  (:require [clj-http.client :as client]
            [clojure.pprint :as pprint]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            ))


(def number-of-pages 60)
(def api-key "")
(def username "")

(defstruct artist :name :mbid :playcount)

(defn zip-str [url]
  (zip/xml-zip (xml/parse url)))

(defn pull-page [page-number]
  (let [url (format  "http://ws.audioscrobbler.com/2.0/?method=library.getartists&api_key=%s&user=%s&page=%d" api-key username page-number)]
    (zip-str url)))

(defn pull-tags-for-artist [artist]
  (if (artist :mbid)
    (let [url (format "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&mbid=%s&api_key=%s" (artist :mbid) api-key)]
      (zip-str url)))
    (let [url (format "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist=%s&api_key=%s" (artist :name) api-key)]
      (zip-str url)))

(defn elements-by-tag-name [node tag-name]
  (for [t (:content node) :when (= tag-name (:tag t))] t))

(defn parse-page [page-number]
  (let [page (pull-page page-number)
        root (first page)]
    (for [artist-tag (xml-seq root) :when (= :artist (:tag artist-tag))]
      (let [name-tag (first (elements-by-tag-name artist-tag :name))
            playcount-tag (first (elements-by-tag-name artist-tag :playcount))
            mbid-tag (first (elements-by-tag-name artist-tag :mbid))]
        (struct artist
                (first (name-tag :content))
                (first (mbid-tag :content))
                (Integer/parseInt (first (playcount-tag :content))))))))

(defn parse-tags-for-artist [artist]
  (let [page (pull-tags-for-artist artist)
        root (first page)]
    (apply hash-map
           (apply concat
                  (remove nil?
                    (for [tag-tag (xml-seq root) :when (= :tag (:tag tag-tag))]
                      (let [name-tag (first (elements-by-tag-name tag-tag :name))
                            count-tag (first (elements-by-tag-name tag-tag :count))
                            tag-count (Integer/parseInt (first (count-tag :content)))]
                        (if-not (zero? tag-count)
                          [(first (name-tag :content)) (* tag-count (artist :playcount))]))))))))

(defn aggregate-tags [artists]
  (apply merge-with + (map parse-tags-for-artist artists)))

(defn main []
  (let [artists (parse-page 1)]
    (pprint/pprint (aggregate-tags artists))))
