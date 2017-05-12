(ns csv-pie.core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [cljsjs.chartjs]))

(enable-console-print!)

(def styles
  {:purple        "#a463f2"
   :app           "f6 cf w-100 pa3"
   :a             "b link light-purple hover-near-black"
   :h             "ma0 light-purple"
   :col           "fl w-100 w-100-m w-third-ns pa4 pb2"
   :table         "f6 w-100 mw8 center collapse dt dt--fixed"
   :th            "tl pa0 ba b--light-purple"
   :td            "ba pa0 b--light-purple hover-bg-white bg-animate"
   :input         "pa2 w-100 ma0 input-reset bn bg-transparent outline-0"
   :input-focused "pa2 w-100 ma0 input-reset bn bg-white outline-0"})

(def sample-data [["Letter" "Number"]
                  ["Aleph" 1]
                  ["Bet" 1]
                  ["Gimel" 2]
                  ["Dalet" 3]
                  ["He" 5]
                  ["Vav" 8]
                  ["Zayin" 13]
                  ["Het" 21]])

(defn csv->cljs [data]
  "Converts raw string from CSV into nested vectors"
  (map #(str/split % #",")
       (str/split data #"\n")))

(defn cljs->csv [data]
  "Converts ClojureScript vectrors into CSV sting"
  (str/join "\n" (map #(str/join "," %) (vec data))))

(defn get-by-id [id]
  "Gets a DOM node by ID"
  (js/document.getElementById id))

(defn chart-init [id config]
  "Setup chart.js via `Chart` constructor"
  (js/Chart. id (clj->js config)))

(defn parse-file [file input-id form-id max-size]
  "Parses and validates an input file with File API"
  (let [raw-file (aget (clj->js (.-files (get-by-id input-id))) 0)
        filesize (.-size raw-file)
        reader (js/FileReader.)]
    (set! (.-onload reader) #(reset! file (clj->js (.-srcElement.result %)))) ;; Sets the callback for `onload` event
    (if (> filesize max-size)
      (do
        (.reset (clj->js (get-by-id form-id)))              ;; Clears out the form
        (js/alert (str "Sorry, but the file is too big. Max allowed file size is: " max-size "B")))
      (.readAsText reader raw-file))))

(defn parse-int [s]
  (js/parseInt s 10))

(defn string-eq-number? [s]
  (= (str (parse-int s)) (str s)))

(defn encode-file-to-uri [data type]
  "Generates a date URI from a file"
  (str "data:attachment/" type "," (js/encodeURIComponent (clj->js data))))

(def data-atom (r/atom sample-data))

(def file-atom (r/atom nil))

(add-watch file-atom nil #(reset! data-atom (vec (csv->cljs @file-atom))))

(def chart-atom (r/atom nil))

(defn get-chart-config [cols]
  {:type    "pie"
   :data    {:datasets [{:data (rest (last cols))}]
             :labels   (rest (first cols))}
   :options {:responsive false
             :legend     {:display false}
             :tooltips   {:caretSize       0
                          :displayColors   false
                          :backgroundColor (:purple styles)}
             :elements   {:arc {:backgroundColor "#fff"
                                :borderColor     (:purple styles)
                                :borderWidth     1}}}})

(defn LayoutColumn [& children]
  [:div {:class (styles :col)}
   children])

(defn Download [props]
  (let [type (:type props)
        data (:data props)
        filename (:filename props)]
    [:div
     [:a {:class    (:a styles)
          :href     (encode-file-to-uri data type)
          :download (str filename (str "." type))}
      "\uD83D\uDCBE Download"]                              ;; Floppy disk emoji here
     (str " current table as " (str/upper-case type) " file")]))

(defn UploadForm [props]
  (let [max-size (:max-size props)                          ;; Max allowed filesize in bytes
        mime (:allowed-mime props)
        warning (:warning props)]
    [:div
     [:form#form
      [:input#upload {:type      "file"
                      :accept    mime
                      :on-change #(parse-file file-atom "upload" "form" max-size)}]
      [:div.pt2 (str "Max size: " max-size "B")]]
     [:div.pt2.light-purple warning]]))

(defn Header [props]
  [:header.cf.pb4
   [LayoutColumn
    [:h1 {:class (styles :h)} (:title props)]]
   [LayoutColumn
    [UploadForm {:max-size     4096
                 :warning      "Beware! Current table will be replaced with the file contents"
                 :allowed-mime "text/csv"}]]
   [LayoutColumn
    [Download {:data     (cljs->csv @data-atom)
               :type     "csv"
               :filename "table"}]]])

(defn Input [props]
  (let [{y :y
         x :x} (:address props)
        class (:class props)
        value (:value props)
        invalid (and
                  (pos? x)
                  (pos? y)
                  (not (string-eq-number? value)))]
    [:input {:class     (str (styles :input) " " (if invalid
                                                   (str class " bg-washed-red")
                                                   class))
             :type      "text"
             :on-change #(swap! data-atom assoc-in [y x] (-> % .-target .-value))
             :value     value}]))

(defn TableCell [props]
  (let [item (:item props)
        index (:index props)
        valid (:valid props)]
    [:td {:class (styles :td)
          :key   (str index (first item))}
     [Input {:value   (last item)
             :address {:y index
                       :x (first item)}}
      (last item)]]))

(defn Table [props]
  (let [indexed-data (vec (map-indexed vector (:data props)))]
    [:table {:class (styles :table)}
     [:thead
      [:tr
       (for [item (map-indexed vector (get (first indexed-data) 1))]
         (let [index (first item)]
           ^{:key index}
           [:th {:class (styles :th)}
            [Input {:value   (last item)
                    :class   "b"
                    :address {:y 0
                              :x index}}
             item]]))]]
     [:tbody
      (for [item (rest indexed-data)]
        (let [[idx row] item]
          [:tr {:key idx}
           (for [itm (map-indexed vector row)]
             [TableCell {:item  itm
                         :valid true
                         :index idx}])]))]]))

(defn Chart [props]
  (r/create-class
    {:display-name        "Chart"
     :component-did-mount #(reset! chart-atom (chart-init "chart" (:config props)))
     :reagent-render      (fn [] [:canvas#chart])}))

(def cols (r/atom (apply map vector @data-atom)))

(add-watch data-atom :update-canvas
           ;; I have no idea how to make it better for now
           #(do
              (reset! cols (apply map vector @data-atom))
              (.destroy @chart-atom)
              (reset! chart-atom (chart-init "chart" (get-chart-config @cols)))
              (.update @chart-atom)))

(defn App []
  [:div {:class (styles :app)}
   [Header {:title "The 🍰 made of CSV"}]
   [:div.cf.pt2.pb5.bt.b--light-purple
    [LayoutColumn
     [Table {:data @data-atom}]]
    [LayoutColumn
     [Chart {:config (get-chart-config @cols)}]]]])

(r/render [App] (get-by-id "app"))
