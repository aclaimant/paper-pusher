(ns aclaimant.paper-pusher.service
  (:require
    [clojure.string :as string]
    [compojure.core :refer [defroutes ANY GET POST wrap-routes]]
    [outpace.config :refer [defconfig!]]
    [ring.middleware.edn :as edn-mw]
    [ring.middleware.keyword-params :as keyword-params]
    [ring.middleware.params :as params])
  (:import
    [com.itextpdf.text.pdf AcroFields BaseFont PdfReader PdfStamper PdfContentByte ColumnText PdfString PdfName PdfAnnotation PdfContentByte]
    [com.itextpdf.text Paragraph Rectangle Element Phrase BaseColor]
    [java.io ByteArrayOutputStream ByteArrayInputStream]
    [org.apache.commons.io IOUtils]))

(defonce ^:private start-time (atom nil))

(defconfig! api-key)

(defn ^:private make-reader [url]
  (PdfReader. url))

(defn ^:private make-stamper [reader out]
  (PdfStamper. reader out))

(defn ^:private get-fields [stamper]
  (mapv key (.getFields (.getAcroFields stamper))))

(defn ^:private checkbox? [{field-type :type}]
  (= AcroFields/FIELD_TYPE_CHECKBOX field-type))

(defn ^:private get-field-info [stamper field-name]
  (let [acro-fields (.getAcroFields stamper)
        field-position (first (.getFieldPositions acro-fields field-name))
        field-rect (Rectangle. (.-position field-position))]
    {:field-rect field-rect
     :page (.-page field-position)
     :type (.getFieldType acro-fields field-name)
     :options (mapv #(.toString %) (.getAppearanceStates acro-fields field-name))}))

(defn ^:private annotate [stamper field-name field-value]
  (let [{:keys [field-rect options page] :as field-info} (get-field-info stamper field-name)
        annotation (PdfAnnotation/createText
                     (.getWriter stamper)
                     field-rect
                     field-value
                     (if (checkbox? field-info)
                       (format "Name: %s\nValues: %s"
                               field-name
                               (string/join ", " options))
                       field-name)
                     false
                     "Help")]
    (.addAnnotation stamper annotation page)))

(defn ^:private show-text
  [stamper field-name field-value color]
  (let [{:keys [field-rect page] :as field-info} (get-field-info stamper field-name)
        cb (.getOverContent stamper page)
        field-title (if (checkbox? field-info) (str "Checkbox: " field-value) field-value)]
    (.setColorFill cb color)
    (ColumnText/showTextAligned cb Element/ALIGN_LEFT
                                (Phrase. field-title)
                                (+ (.getLeft field-rect) 5) (.getBottom field-rect) 0)))

(defn ^:private draw-line [cb ratio [x y x' y'] field-rect]
  (doto cb
    (.setLineWidth (* ratio 3.0))
    (.moveTo (+ x (.getLeft field-rect))
             (+ (* -1 y) (.getTop field-rect)))
    (.lineTo (+ x' (.getLeft field-rect))
             (+ (* -1 y') (.getTop field-rect)))
    (.stroke)))

(defn ^:private normalize-line [[line-x line-y line-x' line-y'] [offset-x offset-y] [x-ratio y-ratio]]
  (let [[x y x' y'] [(- line-x offset-x) (- line-y offset-y) (- line-x' offset-x) (- line-y' offset-y)]]
    [(double (* x x-ratio))
     (double (* y x-ratio))
     (double (* x' y-ratio))
     (double (* y' y-ratio))]))

(defn ^:private determine-ratios [[offset-x offset-y max-x max-y] [fillable-width fillable-height]]
  (let [[input-width input-height] [(- max-x offset-x) (- max-y offset-y)]
        width-ratio (/ fillable-width input-width)
        height-ratio (/ fillable-height input-height)
        aspect-ratio (/ input-width input-height)]
    (if (< height-ratio width-ratio)
      [height-ratio (/ (* fillable-height aspect-ratio) input-width) (/ fillable-height input-height)]
      [width-ratio (/ fillable-width input-width) (/ (/ fillable-width aspect-ratio) input-height)])))

(defn ^:private zero-length? [[x1 y1 x2 y2]]
  (and (= x1 x2) (= y1 y2)))

(defn ^:private bounding-box [lines]
  (reduce (fn [[min-x min-y max-x max-y] [x y x' y']]
            [(min min-x x x')
             (min min-y y y')
             (max max-x x x')
             (max max-y y y')])
          [Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY 0 0]
          (remove zero-length? lines)))

(defn adjust-lines [lines x y]
  (let [bounds (bounding-box lines)
        [ratio x-ratio y-ratio] (determine-ratios bounds [x y])]
    [ratio (map #(normalize-line % bounds [x-ratio y-ratio]) lines)]))

(defn ^:private draw-image
  [stamper field-name {:keys [lines]}]
  (let [{:keys [field-rect page] :as _field-info} (get-field-info stamper field-name)
        cb (.getOverContent stamper page)
        [ratio adjusted-lines] (adjust-lines lines (.getWidth field-rect) (.getHeight field-rect))]
    (doseq [line adjusted-lines]
      (draw-line cb ratio line field-rect))))

(def ^:private colors
  {:transparent (BaseColor. 1.0 1.0 1.0 0.0)})

(defn ^:private set-fields [stamper values]
  (let [fields (.getAcroFields stamper)
        base-font (BaseFont/createFont "resources/DejaVuSans.ttf" BaseFont/IDENTITY_H BaseFont/EMBEDDED)]
    (.addSubstitutionFont fields base-font)
    (doseq [[field-name field-value-or-map] values
            :when (get (.getFields fields) (name field-name))
            :let [{:keys [value color lines]} (if (map? field-value-or-map) field-value-or-map {:value field-value-or-map})
                  field-name (name field-name)
                  field-info (get-field-info stamper field-name)
                  field-value (str value)
                  color (when color (colors color))]]
      (cond
        lines (draw-image stamper field-name field-value-or-map)
        color (show-text stamper field-name field-value color)
        (checkbox? field-info) (.setField fields field-name field-value true)
        :else (.setField fields field-name field-value)))))

(defn ^:private form-fields [pdf-url]
  (with-open [out (ByteArrayOutputStream.)
              reader (make-reader pdf-url)
              stamper (make-stamper reader out)]
    (get-fields stamper)))

(defn ^:private preview-fields [pdf-url]
  (with-open [out (ByteArrayOutputStream.)]
    (with-open [reader (make-reader pdf-url)
                stamper (make-stamper reader out)]
      (doseq [field-name (get-fields stamper)]
        (annotate stamper field-name field-name)
        (show-text stamper field-name field-name BaseColor/BLUE)))
    (ByteArrayInputStream. (.toByteArray out))))

(defn ^:private with-field-values [pdf-url values]
  (with-open [out (ByteArrayOutputStream.)]
    (with-open [reader (make-reader pdf-url)
                stamper (make-stamper reader out)]
      (set-fields stamper values)
      (.setFormFlattening stamper true))
    (ByteArrayInputStream. (.toByteArray out))))

(defroutes public-routes
  (GET "/paper-pusher/status" []
    {:status 200
     :body (format "Been pushing papers for %dms" (- (System/currentTimeMillis) @start-time))}))

(defn ^:private pdf-url [{:keys [pdf-url request-key]}]
  (if request-key
    (str pdf-url "?key=" request-key)
    pdf-url))

(defroutes protected-routes
  (POST "/paper-pusher/push" {params :params}
    {:status 200
     :headers {"Content-Type" "application/pdf"}
     :body (with-field-values (pdf-url params) (:values params))})
  (POST "/paper-pusher/preview" {params :params}
    (let [pdf-url (pdf-url params)]
      {:status 200
       :headers {"Content-Type" "application/pdf"}
       :body (preview-fields pdf-url)}))
  (POST "/paper-pusher/form-fields" {params :params}
    (let [pdf-url (pdf-url params)]
      {:status 200
       :headers {"Content-Type" "application/edn"}
       :body (pr-str (form-fields pdf-url))})))

(defn ^:private wrap-api-key [handler]
  (fn [req]
    (let [request-key (get-in req [:headers "x-paper-pusher-key"])]
      (cond
        (nil? request-key) {:status 401}
        (= request-key api-key) (handler req)
        :else {:status 403}))))

(defn ^:private wrap-errors [handler]
  (fn [e]
    (try
      (handler e)
      (catch Throwable ex
        (println "An error has occurred" (.getMessage ex))
        (.printStackTrace ex)
        {:status 500
         :body "An error has occurred."}))))

(defroutes service-routes
  public-routes
  (wrap-routes protected-routes wrap-api-key)
  (ANY "*" []
    {:status 404}))

(def app
  (-> #'service-routes
      ;wrap-api-key
      edn-mw/wrap-edn-params
      keyword-params/wrap-keyword-params
      params/wrap-params
      wrap-errors))

(defn main []
  (set! (. PdfReader unethicalreading) true)
  (println "Initialized paper-pusher")
  (reset! start-time (System/currentTimeMillis)))
