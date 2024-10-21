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
    [com.itextpdf.text.exceptions InvalidPdfException]
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

(defn ^:private get-field-infos [stamper field-name]
  (let [acro-fields (.getAcroFields stamper)
        field-positions (.getFieldPositions acro-fields field-name)]
    (for [field-position field-positions
          :let [field-rect (Rectangle. (.-position field-position))]]
      {:field-rect field-rect
       :page (.-page field-position)
       :type (.getFieldType acro-fields field-name)
       :options (mapv #(.toString %) (.getAppearanceStates acro-fields field-name))})))

(defn ^:private annotate [stamper field-name field-value]
  (doseq [{:keys [field-rect options page] :as field-info} (get-field-infos stamper field-name)
          :let [annotation (PdfAnnotation/createText
                            (.getWriter stamper)
                            field-rect
                            field-value
                            (if (checkbox? field-info)
                              (format "Name: %s\nValues: %s"
                                      field-name
                                      (string/join ", " options))
                              field-name)
                            false
                            "Help")]]
    (.addAnnotation stamper annotation page)))

(defn ^:private show-text
  [stamper field-name field-value color]
  (doseq [{:keys [field-rect page] :as field-info} (get-field-infos stamper field-name)
          :let [cb (.getOverContent stamper page)
                field-title (if (checkbox? field-info) (str "Checkbox: " field-value) field-value)]]
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
        ;; in the rare event of a perfectly straight line, vertical or
        ;; horizontal, it would be impossible to determine ratios because we
        ;; would be dividing by zero (e.g. max-y - offset-y is 0 for a perfectly
        ;; horizontal line). instead, just use the fillable width or height,
        ;; which will result in a ratio of 1.
        input-width (if (zero? input-width) fillable-width input-width)
        input-height (if (zero? input-height) fillable-height input-height)

        width-ratio (/ fillable-width input-width)
        height-ratio (/ fillable-height input-height)
        aspect-ratio  (/ input-width input-height)]
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
    [ratio (->> lines
                (remove zero-length?)
                (map #(normalize-line % bounds [x-ratio y-ratio])))]))

(comment
  (defn to-svg
    ([lines] (to-svg lines "black"))
    ([lines stroke]
     (map
       (fn [[x y x' y']]
         (format "<line x1='%s' y1='%s' x2='%s' y2='%s' stroke='%s' />" x y x' y' stroke))
       lines)))

  (def horizontal [[66, 162.3125, 66, 162.3125], [66, 162.3125, 68, 162.3125], [68, 162.3125, 74, 162.3125], [74, 162.3125, 113, 162.3125], [113, 162.3125, 197, 162.3125], [197, 162.3125, 292, 162.3125], [292, 162.3125, 352, 162.3125], [352, 162.3125, 405, 162.3125]])
  (adjust-lines horizontal 400 400)

  (def bounds [66 162.3125 405 162.3125])

  (determine-ratios [66 162.3125 405 162.3125] [1000 1000])

  ;; problem signature (perfectly horizontal)
  (apply str (to-svg horizontal))
  "<line x1='66' y1='162.3125' x2='66' y2='162.3125' stroke='black' /><line x1='66' y1='162.3125' x2='68' y2='162.3125' stroke='black' /><line x1='68' y1='162.3125' x2='74' y2='162.3125' stroke='black' /><line x1='74' y1='162.3125' x2='113' y2='162.3125' stroke='black' /><line x1='113' y1='162.3125' x2='197' y2='162.3125' stroke='black' /><line x1='197' y1='162.3125' x2='292' y2='162.3125' stroke='black' /><line x1='292' y1='162.3125' x2='352' y2='162.3125' stroke='black' /><line x1='352' y1='162.3125' x2='405' y2='162.3125' stroke='black' />"

  ;; problem signature normalized
  (apply str (to-svg (second (adjust-lines horizontal 200 200)) "blue"))
  "<line x1='0.0' y1='0.0' x2='1.179941002949853' y2='0.0' stroke='blue' /><line x1='1.179941002949853' y1='0.0' x2='4.71976401179941' y2='0.0' stroke='blue' /><line x1='4.71976401179941' y1='0.0' x2='27.72861356932153' y2='0.0' stroke='blue' /><line x1='27.72861356932153' y1='0.0' x2='77.28613569321534' y2='0.0' stroke='blue' /><line x1='77.28613569321534' y1='0.0' x2='133.3333333333333' y2='0.0' stroke='blue' /><line x1='133.3333333333333' y1='0.0' x2='168.7315634218289' y2='0.0' stroke='blue' /><line x1='168.7315634218289' y1='0.0' x2='200.0' y2='0.0' stroke='blue' />"

  ;; normal signature (valencia!)
  (apply str (to-svg [[70, 55.3125, 70, 57.3125], [70, 57.3125, 71, 85.3125], [71, 85.3125, 71, 113.3125], [71, 113.3125, 73, 170.3125], [73, 170.3125, 73, 195.3125], [73, 195.3125, 73, 208.3125], [73, 208.3125, 73, 209.3125], [130, 98.3125, 130, 97.3125], [130, 97.3125, 130, 97.3125], [130, 97.3125, 142, 97.3125], [142, 97.3125, 147, 97.3125], [147, 97.3125, 163, 96.3125], [163, 96.3125, 196, 91.3125], [196, 91.3125, 224, 88.3125], [224, 88.3125, 241, 87.3125], [241, 87.3125, 248, 87.3125], [248, 87.3125, 252, 87.3125], [202, 161.3125, 202, 161.3125], [202, 161.3125, 208, 161.3125], [208, 161.3125, 241, 160.3125], [241, 160.3125, 277, 160.3125], [277, 160.3125, 306, 160.3125], [306, 160.3125, 328, 160.3125], [328, 160.3125, 337, 160.3125]]))
  "<line x1='70' y1='55.3125' x2='70' y2='57.3125' stroke='black' /><line x1='70' y1='57.3125' x2='71' y2='85.3125' stroke='black' /><line x1='71' y1='85.3125' x2='71' y2='113.3125' stroke='black' /><line x1='71' y1='113.3125' x2='73' y2='170.3125' stroke='black' /><line x1='73' y1='170.3125' x2='73' y2='195.3125' stroke='black' /><line x1='73' y1='195.3125' x2='73' y2='208.3125' stroke='black' /><line x1='73' y1='208.3125' x2='73' y2='209.3125' stroke='black' /><line x1='130' y1='98.3125' x2='130' y2='97.3125' stroke='black' /><line x1='130' y1='97.3125' x2='130' y2='97.3125' stroke='black' /><line x1='130' y1='97.3125' x2='142' y2='97.3125' stroke='black' /><line x1='142' y1='97.3125' x2='147' y2='97.3125' stroke='black' /><line x1='147' y1='97.3125' x2='163' y2='96.3125' stroke='black' /><line x1='163' y1='96.3125' x2='196' y2='91.3125' stroke='black' /><line x1='196' y1='91.3125' x2='224' y2='88.3125' stroke='black' /><line x1='224' y1='88.3125' x2='241' y2='87.3125' stroke='black' /><line x1='241' y1='87.3125' x2='248' y2='87.3125' stroke='black' /><line x1='248' y1='87.3125' x2='252' y2='87.3125' stroke='black' /><line x1='202' y1='161.3125' x2='202' y2='161.3125' stroke='black' /><line x1='202' y1='161.3125' x2='208' y2='161.3125' stroke='black' /><line x1='208' y1='161.3125' x2='241' y2='160.3125' stroke='black' /><line x1='241' y1='160.3125' x2='277' y2='160.3125' stroke='black' /><line x1='277' y1='160.3125' x2='306' y2='160.3125' stroke='black' /><line x1='306' y1='160.3125' x2='328' y2='160.3125' stroke='black' /><line x1='328' y1='160.3125' x2='337' y2='160.3125' stroke='black' />"
  (apply str (to-svg (second (adjust-lines [[70, 55.3125, 70, 57.3125], [70, 57.3125, 71, 85.3125], [71, 85.3125, 71, 113.3125], [71, 113.3125, 73, 170.3125], [73, 170.3125, 73, 195.3125], [73, 195.3125, 73, 208.3125], [73, 208.3125, 73, 209.3125], [130, 98.3125, 130, 97.3125], [130, 97.3125, 130, 97.3125], [130, 97.3125, 142, 97.3125], [142, 97.3125, 147, 97.3125], [147, 97.3125, 163, 96.3125], [163, 96.3125, 196, 91.3125], [196, 91.3125, 224, 88.3125], [224, 88.3125, 241, 87.3125], [241, 87.3125, 248, 87.3125], [248, 87.3125, 252, 87.3125], [202, 161.3125, 202, 161.3125], [202, 161.3125, 208, 161.3125], [208, 161.3125, 241, 160.3125], [241, 160.3125, 277, 160.3125], [277, 160.3125, 306, 160.3125], [306, 160.3125, 328, 160.3125], [328, 160.3125, 337, 160.3125]] 200 200)) "blue"))
  "<line x1='0.0' y1='0.0' x2='0.0' y2='1.49812734082397' stroke='blue' /><line x1='0.0' y1='1.49812734082397' x2='0.749063670411985' y2='22.47191011235955' stroke='blue' /><line x1='0.749063670411985' y1='22.47191011235955' x2='0.749063670411985' y2='43.44569288389513' stroke='blue' /><line x1='0.749063670411985' y1='43.44569288389513' x2='2.2471910112359548' y2='86.14232209737827' stroke='blue' /><line x1='2.247191011235955' y1='86.14232209737827' x2='2.2471910112359548' y2='104.8689138576779' stroke='blue' /><line x1='2.247191011235955' y1='104.8689138576779' x2='2.2471910112359548' y2='114.6067415730337' stroke='blue' /><line x1='2.247191011235955' y1='114.6067415730337' x2='2.2471910112359548' y2='115.35580524344569' stroke='blue' /><line x1='44.9438202247191' y1='32.20973782771535' x2='44.9438202247191' y2='31.46067415730337' stroke='blue' /><line x1='44.9438202247191' y1='31.46067415730337' x2='53.93258426966292' y2='31.46067415730337' stroke='blue' /><line x1='53.93258426966292' y1='31.46067415730337' x2='57.67790262172284' y2='31.46067415730337' stroke='blue' /><line x1='57.67790262172285' y1='31.46067415730337' x2='69.6629213483146' y2='30.711610486891384' stroke='blue' /><line x1='69.66292134831461' y1='30.711610486891384' x2='94.3820224719101' y2='26.96629213483146' stroke='blue' /><line x1='94.3820224719101' y1='26.96629213483146' x2='115.35580524344569' y2='24.719101123595504' stroke='blue' /><line x1='115.3558052434457' y1='24.719101123595504' x2='128.08988764044943' y2='23.97003745318352' stroke='blue' /><line x1='128.0898876404494' y1='23.97003745318352' x2='133.33333333333331' y2='23.97003745318352' stroke='blue' /><line x1='133.3333333333333' y1='23.97003745318352' x2='136.32958801498125' y2='23.97003745318352' stroke='blue' /><line x1='98.87640449438202' y1='79.4007490636704' x2='103.37078651685393' y2='79.4007490636704' stroke='blue' /><line x1='103.3707865168539' y1='79.4007490636704' x2='128.08988764044943' y2='78.65168539325842' stroke='blue' /><line x1='128.0898876404494' y1='78.65168539325842' x2='155.0561797752809' y2='78.65168539325842' stroke='blue' /><line x1='155.0561797752809' y1='78.65168539325842' x2='176.77902621722845' y2='78.65168539325842' stroke='blue' /><line x1='176.7790262172285' y1='78.65168539325842' x2='193.25842696629212' y2='78.65168539325842' stroke='blue' /><line x1='193.2584269662921' y1='78.65168539325842' x2='199.99999999999997' y2='78.65168539325842' stroke='blue' />"

  ;; another normal signature
  (apply str (to-svg [[59, 140.3125, 52, 149.3125], [52, 149.3125, 50, 152.3125], [50, 152.3125, 50, 152.3125], [50, 152.3125, 107, 100.3125], [107, 100.3125, 128, 92.3125], [128, 92.3125, 137, 91.3125], [137, 91.3125, 144, 91.3125], [144, 91.3125, 152, 92.3125], [152, 92.3125, 173, 99.3125], [173, 99.3125, 193, 107.3125], [193, 107.3125, 204, 110.3125], [204, 110.3125, 224, 114.3125], [224, 114.3125, 246, 116.3125], [246, 116.3125, 266, 117.3125], [266, 117.3125, 276, 118.3125], [276, 118.3125, 289, 118.3125], [289, 118.3125, 295, 118.3125], [295, 118.3125, 296, 118.3125], [296, 118.3125, 296, 118.3125]]))
  "<line x1='59' y1='140.3125' x2='52' y2='149.3125' stroke='black' /><line x1='52' y1='149.3125' x2='50' y2='152.3125' stroke='black' /><line x1='50' y1='152.3125' x2='50' y2='152.3125' stroke='black' /><line x1='50' y1='152.3125' x2='107' y2='100.3125' stroke='black' /><line x1='107' y1='100.3125' x2='128' y2='92.3125' stroke='black' /><line x1='128' y1='92.3125' x2='137' y2='91.3125' stroke='black' /><line x1='137' y1='91.3125' x2='144' y2='91.3125' stroke='black' /><line x1='144' y1='91.3125' x2='152' y2='92.3125' stroke='black' /><line x1='152' y1='92.3125' x2='173' y2='99.3125' stroke='black' /><line x1='173' y1='99.3125' x2='193' y2='107.3125' stroke='black' /><line x1='193' y1='107.3125' x2='204' y2='110.3125' stroke='black' /><line x1='204' y1='110.3125' x2='224' y2='114.3125' stroke='black' /><line x1='224' y1='114.3125' x2='246' y2='116.3125' stroke='black' /><line x1='246' y1='116.3125' x2='266' y2='117.3125' stroke='black' /><line x1='266' y1='117.3125' x2='276' y2='118.3125' stroke='black' /><line x1='276' y1='118.3125' x2='289' y2='118.3125' stroke='black' /><line x1='289' y1='118.3125' x2='295' y2='118.3125' stroke='black' /><line x1='295' y1='118.3125' x2='296' y2='118.3125' stroke='black' /><line x1='296' y1='118.3125' x2='296' y2='118.3125' stroke='black' />"
  (apply str (to-svg (second (adjust-lines [[59, 140.3125, 52, 149.3125], [52, 149.3125, 50, 152.3125], [50, 152.3125, 50, 152.3125], [50, 152.3125, 107, 100.3125], [107, 100.3125, 128, 92.3125], [128, 92.3125, 137, 91.3125], [137, 91.3125, 144, 91.3125], [144, 91.3125, 152, 92.3125], [152, 92.3125, 173, 99.3125], [173, 99.3125, 193, 107.3125], [193, 107.3125, 204, 110.3125], [204, 110.3125, 224, 114.3125], [224, 114.3125, 246, 116.3125], [246, 116.3125, 266, 117.3125], [266, 117.3125, 276, 118.3125], [276, 118.3125, 289, 118.3125], [289, 118.3125, 295, 118.3125], [295, 118.3125, 296, 118.3125], [296, 118.3125, 296, 118.3125]] 200 200)) "blue"))
  "<line x1='7.317073170731707' y1='39.83739837398374' x2='1.6260162601626018' y2='47.154471544715456' stroke='blue' /><line x1='1.626016260162602' y1='47.15447154471545' x2='0.0' y2='49.59349593495936' stroke='blue' /><line x1='0.0' y1='49.59349593495935' x2='46.34146341463415' y2='7.3170731707317085' stroke='blue' /><line x1='46.34146341463415' y1='7.317073170731707' x2='63.41463414634147' y2='0.8130081300813009' stroke='blue' /><line x1='63.41463414634146' y1='0.8130081300813008' x2='70.73170731707317' y2='0.0' stroke='blue' /><line x1='70.73170731707317' y1='0.0' x2='76.42276422764229' y2='0.0' stroke='blue' /><line x1='76.42276422764228' y1='0.0' x2='82.92682926829269' y2='0.8130081300813009' stroke='blue' /><line x1='82.92682926829268' y1='0.8130081300813008' x2='100.00000000000001' y2='6.504065040650407' stroke='blue' /><line x1='100.0' y1='6.504065040650406' x2='116.26016260162604' y2='13.008130081300814' stroke='blue' /><line x1='116.260162601626' y1='13.008130081300813' x2='125.20325203252034' y2='15.447154471544717' stroke='blue' /><line x1='125.2032520325203' y1='15.447154471544716' x2='141.46341463414635' y2='18.699186991869922' stroke='blue' /><line x1='141.4634146341463' y1='18.69918699186992' x2='159.34959349593498' y2='20.325203252032523' stroke='blue' /><line x1='159.349593495935' y1='20.32520325203252' x2='175.609756097561' y2='21.138211382113823' stroke='blue' /><line x1='175.609756097561' y1='21.13821138211382' x2='183.739837398374' y2='21.951219512195124' stroke='blue' /><line x1='183.739837398374' y1='21.95121951219512' x2='194.30894308943093' y2='21.951219512195124' stroke='blue' /><line x1='194.3089430894309' y1='21.95121951219512' x2='199.18699186991873' y2='21.951219512195124' stroke='blue' /><line x1='199.1869918699187' y1='21.95121951219512' x2='200.00000000000003' y2='21.951219512195124' stroke='blue' />"
  )


(defn ^:private draw-image
  [stamper field-name {:keys [lines]}]
  (doseq [{:keys [field-rect page] :as _field-info} (get-field-infos stamper field-name)
          :let [cb (.getOverContent stamper page)
                [ratio adjusted-lines] (adjust-lines lines (.getWidth field-rect) (.getHeight field-rect))]]
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
                  field-info (first (get-field-infos stamper field-name))
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

(defn log-request
  "Logs the pdf url and an optional request-id."
  [route {:keys [pdf-url request-id]}]
  (println (format "[%s] %s: %s" (or request-id "") route pdf-url)))

(defroutes protected-routes
  (POST "/paper-pusher/push" {params :params}
    (log-request "push" params)
    {:status 200
     :headers {"Content-Type" "application/pdf"}
     :body (with-field-values (pdf-url params) (:values params))})
  (POST "/paper-pusher/preview" {params :params}
    (log-request "preview" params)
    (let [pdf-url (pdf-url params)]
      {:status 200
       :headers {"Content-Type" "application/pdf"}
       :body (preview-fields pdf-url)}))
  (POST "/paper-pusher/form-fields" {params :params}
    (log-request "form-fields" params)
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
      (catch InvalidPdfException ex
        (println "An error has occurred" (.getMessage ex))
        (.printStackTrace ex)
        {:status 415
         :body "Unsupported PDF format. Perhaps it is encrypted or corrupted."})
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
