(ns aclaimant.paper-pusher.service
  (:require
    [compojure.core :refer [defroutes GET POST]]
    [outpace.config :refer [defconfig!]]
    [ring.middleware.edn :as edn-mw]
    [ring.middleware.keyword-params :as keyword-params]
    [ring.middleware.params :as params])
  (:import
    [com.itextpdf.text.pdf PdfReader PdfStamper]
    [java.io ByteArrayOutputStream ByteArrayInputStream]
    [org.apache.commons.io IOUtils]))

(defconfig! api-key)

(defn ^:private with-field-values [pdf-url values]
  (let [reader (PdfReader. pdf-url)
        out (ByteArrayOutputStream.)
        stamper (PdfStamper. reader out)
        fields (.getAcroFields stamper)]
    (doseq [[field-name field-value] values]
      (.setField fields (name field-name) field-value))
    (.setFormFlattening stamper true)
    ; TODO: Use with-open to auto-close
    (.close stamper)
    (.close reader)
    (let [data (.toByteArray out)]
      (.close out)
      (ByteArrayInputStream. data))))

(defroutes service-routes
  (POST "/paper-pusher/push" {params :params}
    (println (pr-str params))
    {:status 200
     :body (with-field-values (:pdf-url params) (:values params))}
    ))

(defn ^:private wrap-api-key [handler]
  (fn [req]
    (let [request-key (get-in req [:headers "x-paper-pusher-key"])]
      (cond
        (nil? request-key) {:status 401}
        (= request-key api-key) (handler req)
        :else {:status 403}))))

(def app
  (-> #'service-routes
      wrap-api-key
      edn-mw/wrap-edn-params
      keyword-params/wrap-keyword-params
      params/wrap-params))

(defn main []
  (println "Initialized paper-pusher"))
