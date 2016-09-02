(ns aclaimant.paper-pusher.service
  (:require
    [compojure.core :refer [defroutes ANY GET POST wrap-routes]]
    [outpace.config :refer [defconfig!]]
    [ring.middleware.edn :as edn-mw]
    [ring.middleware.keyword-params :as keyword-params]
    [ring.middleware.params :as params])
  (:import
    [com.itextpdf.text.pdf PdfReader PdfStamper]
    [java.io ByteArrayOutputStream ByteArrayInputStream]
    [org.apache.commons.io IOUtils]))

(defonce ^:private start-time (atom nil))

(defconfig! api-key)

(defn ^:private with-field-values [pdf-url values]
  (let [reader (PdfReader. pdf-url)
        out (ByteArrayOutputStream.)
        stamper (PdfStamper. reader out)
        fields (.getAcroFields stamper)]
    (doseq [[field-name field-value] values]
      (.setField fields (name field-name) (str field-value)))
    (.setFormFlattening stamper true)
    ; TODO: Use with-open to auto-close
    (.close stamper)
    (.close reader)
    (let [data (.toByteArray out)]
      (.close out)
      (ByteArrayInputStream. data))))

(defroutes public-routes
  (GET "/paper-pusher/status" []
    {:status 200
     :body (format "Been pushing papers since for %dms" (- (System/currentTimeMillis) @start-time))}))

(defn ^:private pdf-url [{:keys [pdf-url request-key]}]
  (if request-key
    (str pdf-url "?key=" request-key)
    pdf-url))

(defroutes protected-routes
  (POST "/paper-pusher/push" {params :params}
    {:status 200
     :headers {"Content-Type" "application/pdf"}
     :body (with-field-values (pdf-url params) (:values params))}))

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
  (println "Initialized paper-pusher")
  (reset! start-time (System/currentTimeMillis)))
