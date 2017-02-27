(ns aclaimant.paper-pusher.service
  (:require
    [clojure.string :as string]
    [compojure.core :refer [defroutes ANY GET POST wrap-routes]]
    [outpace.config :refer [defconfig!]]
    [ring.middleware.edn :as edn-mw]
    [ring.middleware.keyword-params :as keyword-params]
    [ring.middleware.params :as params])
  (:import
    [com.itextpdf.text.pdf AcroFields PdfReader PdfStamper PdfContentByte ColumnText PdfString PdfName PdfAnnotation PdfContentByte]
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

(defn ^:private annotate
  ([stamper field-name cb]
   (annotate stamper field-name field-name cb BaseColor/BLUE))
  ([stamper field-name field-value cb color]
   (let [acro-fields (.getAcroFields stamper)
         field-rect (Rectangle. (.-position (first (.getFieldPositions acro-fields field-name))))
         checkbox? (= AcroFields/FIELD_TYPE_CHECKBOX (.getFieldType acro-fields field-name))
         options (when checkbox? (mapv #(.toString %) (.getAppearanceStates acro-fields field-name)))
         field-title (if checkbox? (str "Checkbox: " field-value) field-value)
         annotation (PdfAnnotation/createText
                      (.getWriter stamper)
                      field-rect
                      field-value
                      (if checkbox?
                        (format "Name: %s\nValues: %s"
                                field-name
                                (string/join ", " options))
                        field-name)
                      false
                      "Help")]
     (.setColorFill cb color)
     (when (= color BaseColor/BLUE)
       (.addAnnotation stamper annotation 1))
     (ColumnText/showTextAligned cb Element/ALIGN_LEFT
                                 (Phrase. field-value)
                                 (+ (.getLeft field-rect) 5) (.getBottom field-rect) 0))))

(defn ^:private set-fields [stamper values]
  (let [fields (.getAcroFields stamper)
        cb (.getOverContent stamper 1)]
    (doseq [[field-name field-value] values]
      (if (= (str field-value) "[sig|req|signer0]")
        (annotate stamper (name field-name) (str field-value) cb (BaseColor. 1.0 1.0 1.0 0.0))
        (.setField fields (name field-name) (str field-value))))))

(defn ^:private form-fields [pdf-url]
  (with-open [out (ByteArrayOutputStream.)
              reader (make-reader pdf-url)
              stamper (make-stamper reader out)]
    (get-fields stamper)))

(defn ^:private preview-fields [pdf-url]
  (with-open [out (ByteArrayOutputStream.)]
    (with-open [reader (make-reader pdf-url)
                stamper (make-stamper reader out)]
      (let [canvas (.getOverContent stamper 1)]
        (doseq [f (get-fields stamper)]
          (annotate stamper f canvas))))
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
