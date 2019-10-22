(ns com.wsscode.pathom.connect.ynab
  (:require [clj-http.client :as http]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.connect.ynab.entity-adapter :as e]))

(s/def ::auth-token string?)
(s/def ::path string?)
(s/def ::replace map?)
(s/def ::params map?)
(s/def ::method keyword?)
(s/def ::body any?)

(s/def :ynab.budget/id uuid?)

(s/def :ynab.transaction/amount int?)
(s/def :ynab.transaction/approved boolean?)
(s/def :ynab.transaction/cleared string?)
(s/def :ynab.transaction/date string?)
(s/def :ynab.transaction/deleted boolean?)
(s/def :ynab.transaction/flag-color string?)
(s/def :ynab.transaction/id string?)
(s/def :ynab.transaction/import-id string?)
(s/def :ynab.transaction/matched-transaction-id string?)
(s/def :ynab.transaction/memo string?)
(s/def :ynab.transaction/subtransactions (s/coll-of any?))
(s/def :ynab.transaction/transfer-account-id string?)
(s/def :ynab.transaction/transfer-transaction-id string?)

(def ::ynab-request
  (s/keys :req [::path] :opt [::replace ::params]))

(defn uuid [x]
  (java.util.UUID/fromString x))

(defn replace-vars [template params]
  (str/replace template #"\{\{(.+?)}}" (fn [[_ key]] (str (get params (keyword key))))))

(defn ynab-request [{::keys [auth-token]} {::keys [path replace params method body]}]
  (-> (http/request
        {:method       (or method :get)
         :url          (str "https://api.youneedabudget.com/v1/"
                         (cond-> path replace (replace-vars replace)))
         :as           :json
         :content-type :json
         :headers      {"Authorization" (str "Bearer " auth-token)}
         :query-params params
         :form-params  body})
      :body))

(defn adapt-account [account]
  (-> account
      (e/update-if :transfer_payee_id uuid)
      (e/update-if :id uuid)
      (e/namespaced-keys "ynab.account")))

(pc/defresolver budget-accounts [env {:keys [ynab.budget/id]}]
  {::pc/input  #{:ynab.budget/id}
   ::pc/output [:ynab.budget/accounts]}
  {:ynab.budget/accounts
   (->> (ynab-request env
          {::path    "budgets/{{id}}/accounts"
           ::replace {:id id}})
        :data :accounts
        (mapv adapt-account))})

(defn adapt-transaction [transaction]
  (-> transaction
      (e/namespaced-keys "ynab.transaction")
      (set/rename-keys {:ynab.transaction/account-id    :ynab.account/id
                        :ynab.transaction/account-name  :ynab.account/name
                        :ynab.transaction/category-id   :ynab.category/id
                        :ynab.transaction/category-name :ynab.category/name
                        :ynab.transaction/payee-id      :ynab.payee/id
                        :ynab.transaction/payee-name    :ynab.payee/name})))

(pc/defresolver budget-transactions [env {:keys [ynab.budget/id]}]
  {::pc/input  #{:ynab.budget/id}
   ::pc/params [::since]
   ::pc/output [{:ynab.budget/transactions
                 [:ynab.account/id
                  :ynab.account/name
                  :ynab.category/id
                  :ynab.category/name
                  :ynab.payee/id
                  :ynab.payee/name
                  :ynab.transaction/amount
                  :ynab.transaction/approved
                  :ynab.transaction/cleared
                  :ynab.transaction/date
                  :ynab.transaction/deleted
                  :ynab.transaction/flag-color
                  :ynab.transaction/id
                  :ynab.transaction/import-id
                  :ynab.transaction/matched-transaction-id
                  :ynab.transaction/memo
                  :ynab.transaction/subtransactions
                  :ynab.transaction/transfer-account-id
                  :ynab.transaction/transfer-transaction-id]}]}
  (let [since (-> env :ast :params ::since)]
    {:ynab.budget/transactions
     (->> (ynab-request env
            {::path    "budgets/{{id}}/transactions"
             ::replace {:id id}
             ::params  (cond-> {} since (assoc :since_date since))})
          :data :transactions
          (mapv adapt-transaction)
          (filterv (comp #{"f8e5d032-1808-46d2-bd43-90a8ace37dfe"} :ynab.account/id)))}))

(pc/defmutation create-transaction
  [env {budget-id :ynab.budget/id
        :keys     [ynab.account/id
                   ynab.transaction/date
                   ynab.transaction/amount
                   ynab.payee/name]}]
  {::pc/sym    'ynab.transaction/create
   ::pc/params [:ynab.budget/id
                :ynab.account/id
                :ynab.transaction/date
                :ynab.transaction/amount
                :ynab.payee/name]
   ::pc/output [:ynab.account/id
                :ynab.account/name
                :ynab.category/id
                :ynab.category/name
                :ynab.payee/id
                :ynab.payee/name
                :ynab.transaction/amount
                :ynab.transaction/approved
                :ynab.transaction/cleared
                :ynab.transaction/date
                :ynab.transaction/deleted
                :ynab.transaction/flag-color
                :ynab.transaction/id
                :ynab.transaction/import-id
                :ynab.transaction/matched-transaction-id
                :ynab.transaction/memo
                :ynab.transaction/subtransactions
                :ynab.transaction/transfer-account-id
                :ynab.transaction/transfer-transaction-id]}
  (->> (ynab-request env
         {::method  :post
          ::path    "budgets/{{id}}/transactions"
          ::replace {:id budget-id}
          ::body    {:transaction {:account_id id
                                   :date       date
                                   :amount     amount
                                   :payee_name name
                                   :memo       ""}}})
       :data :transaction
       adapt-transaction))

(def registry
  [budget-accounts budget-transactions create-transaction])
