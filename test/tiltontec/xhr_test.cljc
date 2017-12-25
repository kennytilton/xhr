(ns tiltontec.xhr-test
  (:require
    #?(:clj [clojure.test :refer :all]
       :cljs [cljs.test :refer-macros [deftest is testing]])

    [tiltontec.xhr
     :refer [make-xhr send-xhr xhr-response xhr-send xhr-await xhr-status
             xhr-status-key xhr-resolved xhr-error? xhrfo synaptic-xhr
             xhr-selection xhr-to-map xhr-name-to-map]]

            [clojure.string :as str]
            [clojure.set :as set]
            [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint cl-format]]
            [tiltontec.util.core :refer [pln xor now *plnk-keys*
                                         counts countit counts-reset]]
            [tiltontec.cell.evaluate :refer [c-get c-awaken not-to-be]]

    #?(:clj
            [tiltontec.cell.base :refer :all]
       :cljs [tiltontec.cell.base
              :refer-macros [without-c-dependency]
              :refer [cells-init +pulse+ unbound cpr]])
            [tiltontec.cell.integrity :refer [*dp-log*]]
            [tiltontec.cell.core :refer :all]

    #?(:clj
            [tiltontec.model.core :refer :all :as md]
       :cljs [tiltontec.model.core
              :refer-macros [the-kids mdv!]
              :refer [md-get fasc fm! make md-reset! backdoor-reset!
                      mx-par]
              :as md])

    #?(:clj
            [tiltontec.cell.synapse :refer :all]
       :cljs [tiltontec.cell.synapse
              :refer-macros [with-synapse]
              :refer []])

    #?(:clj
            [tiltontec.cell.observer :refer [fn-obs]]
       :cljs [tiltontec.cell.observer :refer-macros [fn-obs]])

            [cheshire.core :refer :all]

    #?(:clj
            [clj-http.util :as httpu]
       :cljs [cljs-http.util :as httpu])

    #?(:clj
            [clj-http.client :as client]
       :cljs [cljs-http.client :as client])))

(declare cf-await)

(def ae-adderall "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.brand_name:adderall&limit=3")

(deftest fda-adderall-ok
  (let [xhr (xhr-await (send-xhr ae-adderall))]
    (is (= 200 (xhr-status xhr)))
    (is (= 3 (count (:results (xhr-selection xhr)))))
    (let [ae (first (:results (xhr-selection xhr)))]
      (pln :ae (keys ae)))))

(deftest fda-adderall-syntax-error
  (let [xhr (xhr-await (send-xhr (str/replace ae-adderall #"search" "surch")))]
    (is (not= 200 (xhr-status xhr)))))

#_(let [uri ae-adderall]
    (client/get uri
                {:async? true}
                (fn [response]
                  (cpr :xhr-response!!! (:status response) (keys response) uri)
                  (pprint (parse-string (:body response))))
                (fn [exception]
                  ;; (println :exc exception)
                  (println :beankeys!! (keys (bean exception)))
                  ;;(println :bean!! ) (pprint (bean exception))
                  (println :status (:status (:data (bean exception)))
                           :body (parse-string (:body (:data (bean exception))) true))

                  (cpr :error!!!!!)
                  )))

(def ae-aldactone
  "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.brand_name:aldactone&limit=1")

(def ae-brand
  "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.brand_name:~a&limit=~a")

(def ae-ndc
  "https://api.fda.gov/drug/event.json?search=patient.drug.openfda.product_ndc:~alimit=1")

(def lbl-adderall
  "https://api.fda.gov/drug/label.json?search=openfda.product_ndc:57844-0117")

(def recall-novartis
  "https://api.fda.gov/drug/enforcement.json?search=recalling_firm:novartis&limit=3")

;; --- generics ---------
(def drug-label
  "https://api.fda.gov/drug/label.json?search=openfda.product_ndc:~a")

(def mfr-recalls
  "https://api.fda.gov/drug/enforcement.json?search=recalling_firm:~a&limit=~a")

#_(deftest fda-adderall-ok
    (let [xhr (xhr-await (send-xhr ae-adderall))]
      (is (= 200 (xhr-status xhr)))
      (is (= 3 (count (:results (xhr-selection xhr)))))))

(deftest recall-fail
  (let [xhr (xhr-await
              (send-xhr
                (str "https://api.fda.gov/drug/enforcement.json?"
                     (do                                    ;; httpu/url-encode
                       "search=recalling_firm:Teva Woman&limit=1"))))]
    (println :Teva (xhr-status-key xhr)
             (:recalling_firm (first (:results (xhr-selection xhr)))))))

(declare xhr-dump)

(defn patient-drugs [ae]
  (c? (the-kids
        (for [drug (:drug (:patient ae))
              :let [ndc (first (get-in drug [:openfda :product_ndc]))
                    mfr-name (first (get-in drug [:openfda :manufacturer_name]))]]
          (do (countit [:ae :drug])
              (when-not ndc
                (countit :no-ndc)
                #_(pprint drug))
              (make ::md/family
                    :name :patient-drug
                    :ndc ndc
                    :drugindication (:drugindication drug)
                    :medicinalproduct (:medicinalproduct drug)
                    :kids (c? (the-kids
                                (when ndc
                                  (send-xhr :drug-label (cl-format nil drug-label ndc)))
                                (when mfr-name
                                  (send-xhr :mfr-recall (cl-format nil mfr-recalls
                                                                   (str/replace mfr-name #"[',\.]" "")
                                                                   1)))))))))))


(deftest fda-adverse-lite
  ;; for 3 AEs
  ;;   for each drug taken by patient
  ;;     get the labeling
  ;;     get up to 3 recall notices from the manufacturer
  (counts-reset)
  (let [ae-count 1
        brand "adderall"
        top (send-xhr :brand-adv-events (cl-format nil ae-brand brand ae-count)
                      {:brand brand
                       :kids  (c? (when-let [aes (:results (xhr-selection me))]
                                    (countit :aes aes)
                                    (the-kids
                                      (for [ae aes]
                                        (make ::md/family
                                              :name :adverse-event
                                              :ae (select-keys ae [:transmissiondate
                                                                   :sender
                                                                   :serious])
                                              :patient (dissoc (:patient ae) :drug)
                                              :kids (patient-drugs ae))))))})]


    (when (xhr-await top)
      (is (= 200 (xhr-status-key top)))
      (is (= ae-count (count (md-kids top))))
      (doseq [ae (md-kids top)]
        (doseq [drug (md-kids ae)]
          (doseq [info (md-kids drug)]
            (is (some #{(xhr-status-key info)} [200 400 404])))))

      (pprint (xhr-to-map top))

      (println :fini!!!!!!!! @counts))))




(defmethod xhr-name-to-map :drug-label [xhr]
  {:name :drug-label
   :keys (select-keys (first (:results (:body (:response @xhr))))
                      [:effective_time
                       :package_label_principal_display_panel])})

#_(:description
    :nursing_mothers
    :drug_and_or_laboratory_test_interactions
    :clinical_pharmacology
    :effective_time
    :carcinogenesis_and_mutagenesis_and_impairment_of_fertility
    :set_id
    :how_supplied
    :pediatric_use
    :warnings
    :pregnancy
    :information_for_patients
    :spl_product_data_elements
    :contraindications
    :adverse_reactions
    :drug_interactions
    :labor_and_delivery
    :id
    :dosage_and_administration
    :openfda
    :precautions
    :indications_and_usage
    :laboratory_tests
    :spl_unclassified_section
    :package_label_principal_display_panel
    :version
    :spl_unclassified_section_table
    :general_precautions
    :overdosage)

(defmethod xhr-name-to-map :mfr-recall [xhr]
  {:name :mfr-recall
   :keys (select-keys (first (:results (:body (:response @xhr))))
                      [:report_date
                       :reason_for_recall
                       :product_description])})

#_(:postal_code
    :report_date
    :address_2
    :code_info
    :address_1
    :city
    :recall_initiation_date
    :center_classification_date
    :state
    :reason_for_recall
    :event_id
    :classification
    :product_type
    :recall_number
    :more_code_info
    :product_quantity
    :status
    :product_description
    :initial_firm_notification
    :voluntary_mandated
    :recalling_firm
    :openfda
    :country
    :distribution_pattern)

(defmacro getxhr [id uri & child-xhrs]
  `(make-xhr ~uri
             {:send? true
              :name  ~id
              :kids  (c? ~@child-xhrs)}))

(defn xhr-dump
  ([xhr] (xhr-dump "untagged" xhr))
  ([tag xhr] (if (= 200 (xhr-status-key xhr))
               (case (:name @xhr)
                 :drug-label (cpr tag (:name @xhr) (:dbg @xhr)
                                  :status (xhr-status-key xhr)
                                  :drug-label-warning (first (:warnings (first (:results (md-get xhr :selection))))))
                 :mfr-recall (cpr tag (:name @xhr) (:dbg @xhr)
                                  :status (xhr-status-key xhr)
                                  (first (:results (md-get xhr :selection)))
                                  :sel (select-keys (first (:results (md-get xhr :selection)))
                                                    [:state :reason_for_recall]))
                 (cpr tag :xhr-dump-unknown (:name @xhr) (:dbg @xhr)))
               (do
                 (cpr tag :xhr-dump-fail! tag (xhr-status-key xhr) (:uri @xhr))))))

(deftest xhr-sequentially
  ;; kick off one at a time waiting for the prior to complete before sending next.
  (cells-init)

  (let [xhrs (c?+ [:slot :synch!
                   :obs (fn-obs (when new
                                  (println :fini!!! (map xhrfo new))
                                  (is (= 3 (count new)))
                                  (is (every? #(= :responded (xhr-status-key %)) new))))]

                  (when-let [google (xhr-resolved (with-synapse (:s-goog)
                                                                (send-xhr "http://google.com")))]

                    (when-let [yahoo (xhr-resolved (with-synapse (:s-yahoo)
                                                                 (send-xhr "http://yahoo.com")))]
                      (when-let [youtube (xhr-resolved (with-synapse (:s-tube)
                                                                     (send-xhr "http://youtube.com")))]
                        (cpr :youtube! (xhrfo youtube))
                        (list google yahoo youtube)))))]

    (let [r (cf-await xhrs)]
      (is (not (nil? r)))
      (when r
        (is (= 3 (count r)))
        (is (every? #(= :responded (xhr-status-key %)) r)))
      (println :topfini (map xhrfo (c-get xhrs))))))

(deftest xhr-parallel-one-result
  ;; make a list of requests and produce a list of the responses when all three have completed.
  (cells-init)
  (let [sites (do ["http://google.com" "http://yahoo.com" "http://youtube.com"])
        xhrs (c? (when-let [xhrs (with-synapse (:make-xhrs [])
                                               (map send-xhr sites))]
                   (when (every? #(some #{(xhr-status-key %)} [:responded :error]) xhrs)
                     xhrs)))]
    (let [r (cf-await xhrs)]
      (is (not (nil? r)))
      (when r
        (is (= 3 (count r)))
        (is (every? #(= :responded (xhr-status-key %)) r))))))

(deftest xhr-if-error-else
  ;; do an if-else based on error or success of first xhr
  ;;
  ;; also, we introduce some syntactic sugar: synaptic-xhr
  ;;
  (cells-init)
  (letfn [(cx-if-else [goog-uri]
            (c? (when-let [google (synaptic-xhr :s-goog goog-uri)]
                  (if (xhr-error? google)
                    (when-let [yahoo (synaptic-xhr :s-yahoo "http://yahoo.com")]
                      (cpr :goog-error-try-yahoo)
                      (list yahoo))
                    (when-let [youtube (synaptic-xhr :s-youtube "http://youtube.com")]
                      (cpr :goog-ok-add-youtube)
                      (list google youtube))))))]
    (let [cx-ok (cx-if-else "http://google.com")]
      (let [r (cf-await cx-ok)]
        (is (= 2 (count r)))
        (is (some #(.contains (md-get % :uri) "google") r))
        (is (some #(.contains (md-get % :uri) "youtube") r))))
    (let [cx-ok (cx-if-else "http://googlexxxxxx.com")]
      (let [r (cf-await cx-ok)]
        (is (= 1 (count r)))
        (is (some #(.contains (md-get % :uri) "yahoo") r))))))



(deftest xhr-send-group-get-as-received
  ;; kick off  requests and return them one at a time in order received.
  (cells-init)
  (let [sites ["http://google.com" "http://yahoo.com" "http://youtube.com"]
        responses (atom [])
        h (let [open-xhrs (atom nil)]
            (c? (when-let [xhrs (with-synapse (:make-xhrs)
                                              (reset! open-xhrs (set (map send-xhr sites))))]
                  (let [done (filter #(some #{(xhr-status-key %)} [:responded :error]) @open-xhrs)]
                    (assert (>= 1 (count done)) "done count NG")
                    (reset! open-xhrs (set/difference @open-xhrs done))
                    (first done)))))
        h1 (c?+ [:obs (fn-obs
                        (when new
                          (println :got-xhr!!!!!! new)
                          (swap! responses conj new)))]
                (when-let [xhr (c-get h)]
                  (xhrfo xhr)))]
    (dotimes [_ 3]
      (cf-await h1))
    (Thread/sleep 2000)
    (println :responses @responses)
    (is (= 3 (count @responses)))))

(deftest xhr-tree-simple
  (binding [*plnk-keys* [:xhr]]
    (let [top (make-xhr "http://google.com"
                        {:send? false
                         :kids  (c? (cpr :kidrule!!!!!!)
                                    (when-let [parent (xhr-resolved me)]
                                      (the-kids
                                        (make-xhr "http://yahoo.com"
                                                  {:par   me
                                                   :send? true})
                                        (make-xhr "http://youtube.com"
                                                  {:par   me
                                                   :send? true}))))})]

      (xhr-send top)
      (when (xhr-await top)
        (pln :waited!!!!!!!!!)
        (when (xhr-resolved top)
          (pln :testing!!!!!!!!!!!!!!)
          (is (= :responded (xhr-status-key top)))
          (is (= 2 (count (md-kids top))))
          (doseq [k (md-kids top)]
            (xhr-await k)
            (do                                             ;; when (xhr-resolved k)
              (is (= :responded (xhr-status-key k))))))
        (println :fini!!!!!!!!)))))



;#_(binding [*dp-log* true]
;    ;; ill-advised example here emphasizing this is purgatory, not heaven: the first xhr never renders
;    ;; because we quickly change the site triggering the rule to run, which first disconnects the outer cell
;    ;; from all dependencies including the first XHR, which then no longer propagates to the outer rule
;    ;; because it sees it is no longer a dependency.
;    ;;
;    ;; Moral: see examples above where we either process multiple XHRs in parallel or in sequence by letting each
;    ;; complete before hitting the next. It is actually cool that a changing dependency triggers a complex collextion
;    ;; of Ajax request results to start over from the exact point of the dependency, but for now the Matrix XHR module
;    ;; cannot save us from having to think about outstanding requests still in transit.
;
;    (cells-init)
;    (let [site (c-in "http://google.com" :slot :site)
;          h (c?+ [:slot :htop
;                  :obs (fn-obs (println :new-async-html!!!!! new :old old))]
;                 (pln :h-top!!!!!!!!!!!!!!!!!!!!!!! @sends)
;                 (when-let [xhr (with-synapse (:home-page [])
;                                  ;; the dependency on site will trigger a re-run of
;                                  ;; this synapse generator, displacing the old
;                                  (pln :syn-rule-makes+sends-new-xhr!!!!!!! @sends (type cache))
;                                  (when-not (= cache unbound)
;                                    (not-to-be cache))
;                                  (swap! sends inc)
;                                  (let [s (c-get site)]
;                                    (send-xhr s)))]
;
;                   (when-let [r (xhr-response xhr)]
;                     (pln :the-html-rule-returns!!!!!! r)
;                     (str (c-get site) " = <h1>" (subs (:body r) 0 40) "</h1>"))))]
;      (cpr :kickoff (c-get h))
;
;      (cpr :changing-site!!!!!!!!!!!!!!!!!!)
;      (c-reset! site "http://yahoo.com")))

(defn cf-await
  ([c] (cf-await c 10))
  ([c max-seconds]
   (println :cf-waiting!!!!!!)
   (loop [x 0]
     (let [r (c-get c)]
       (cond
         r (do (print :bingor (xhrfo r)) r)
         (< x 10) (do
                    (println :sleeping x)
                    (Thread/sleep 1000)
                    (recur (inc x)))
         :default (do (println :maxo)
                      nil))))))

