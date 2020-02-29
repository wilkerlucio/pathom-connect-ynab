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

(s/def ::ynab-request
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

(defn adapt-budget [budget]
  (-> budget
      (e/update-if :id uuid)
      (e/namespaced-keys "ynab.budget")
      (e/pull-namespaced :ynab.budget/currency-format "ynab.currency-format")))

(pc/defresolver budgets [env {:keys []}]
  {::pc/output [{:ynab/budgets
                 [{:ynab.budget/date-format [:format]}
                  :ynab.budget/first-month
                  :ynab.budget/id
                  :ynab.budget/last-modified-on
                  :ynab.budget/last-month
                  :ynab.budget/name
                  :ynab.currency-format/currency-symbol
                  :ynab.currency-format/decimal-digits
                  :ynab.currency-format/decimal-separator
                  :ynab.currency-format/display-symbol
                  :ynab.currency-format/example-format
                  :ynab.currency-format/group-separator
                  :ynab.currency-format/iso-code
                  :ynab.currency-format/symbol-first]}]
   }
  {:ynab/budgets
   (->> (ynab-request env
          {::path "budgets"})
        :data :budgets
        (mapv adapt-budget))})

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

(defn safe-uuid
  [s]
  (try
    (uuid s)
    (catch Throwable e
      s)))

(defn adapt-transaction [transaction]
  (-> transaction
      (e/namespaced-keys "ynab.transaction")
      (set/rename-keys {:ynab.transaction/account-id    :ynab.account/id
                        :ynab.transaction/account-name  :ynab.account/name
                        :ynab.transaction/category-id   :ynab.category/id
                        :ynab.transaction/category-name :ynab.category/name
                        :ynab.transaction/payee-id      :ynab.payee/id
                        :ynab.transaction/payee-name    :ynab.payee/name})
      (e/update-if :ynab.account/id safe-uuid)
      (e/update-if :ynab.category/id safe-uuid)
      (e/update-if :ynab.payee/id safe-uuid)))

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
                  :ynab.transaction/transfer-transaction-id
                  :ynab.budget/id]}]}
  (let [since (-> env :ast :params ::since)]
    {:ynab.budget/transactions
     (->> (ynab-request env
            {::path    "budgets/{{id}}/transactions"
             ::replace {:id id}
             ::params  (cond-> {} since (assoc :since_date since))})
          :data :transactions
          (mapv (comp #(assoc % :ynab.budget/id id) adapt-transaction)))}))

(defn adapt-category [category]
  (-> category
      (e/namespaced-keys "ynab.category")
      (set/rename-keys {:ynab.category/category-group-id :ynab.category-group/id})))

(defn adapt-category-group [category-group]
  (-> category-group
      (update :categories (partial mapv adapt-category))
      (e/namespaced-keys "ynab.category-group")
      (set/rename-keys {})))

(pc/defresolver budget-category-groups [env {:keys [ynab.budget/id]}]
  {::pc/input  #{:ynab.budget/id}
   ::pc/output [{:ynab.budget/category-groups
                 [{:ynab.category-group/categories
                   [:ynab.category-group/id
                    :ynab.category/activity
                    :ynab.category/balance
                    :ynab.category/budgeted
                    :ynab.category/deleted
                    :ynab.category/goal-creation-month
                    :ynab.category/goal-percentage-complete
                    :ynab.category/goal-target
                    :ynab.category/goal-target-month
                    :ynab.category/goal-type
                    :ynab.category/hidden
                    :ynab.category/id
                    :ynab.category/name
                    :ynab.category/note
                    :ynab.category/original-category-group-id]}
                  :ynab.category-group/deleted
                  :ynab.category-group/hidden
                  :ynab.category-group/id
                  :ynab.category-group/name]}]}
  {:ynab.budget/category-groups
   (->> (ynab-request env
          {::path    "budgets/{{id}}/categories"
           ::replace {:id id}})
        :data :category_groups
        (mapv adapt-category-group))})

(pc/defresolver budget-categories [env {:keys [ynab.budget/category-groups]}]
  {::pc/input  #{:ynab.budget/category-groups}
   ::pc/output [{:ynab.budget/categories
                 [:ynab.category-group/id
                  :ynab.category/activity
                  :ynab.category/balance
                  :ynab.category/budgeted
                  :ynab.category/deleted
                  :ynab.category/goal-creation-month
                  :ynab.category/goal-percentage-complete
                  :ynab.category/goal-target
                  :ynab.category/goal-target-month
                  :ynab.category/goal-type
                  :ynab.category/hidden
                  :ynab.category/id
                  :ynab.category/name
                  :ynab.category/note
                  :ynab.category/original-category-group-id]}]}
  {:ynab.budget/categories (into [] (mapcat :ynab.category-group/categories) category-groups)})

(pc/defresolver budget-from-id [env {:keys [ynab.budget/id
                                            ynab/budgets]}]
  {::pc/input  #{:ynab.budget/id
                 :ynab/budgets}
   ::pc/output [{:ynab.budget/date-format [:format]}
                :ynab.budget/first-month
                :ynab.budget/id
                :ynab.budget/last-modified-on
                :ynab.budget/last-month
                :ynab.budget/name
                :ynab.currency-format/currency-symbol
                :ynab.currency-format/decimal-digits
                :ynab.currency-format/decimal-separator
                :ynab.currency-format/display-symbol
                :ynab.currency-format/example-format
                :ynab.currency-format/group-separator
                :ynab.currency-format/iso-code
                :ynab.currency-format/symbol-first]}
  (some #(if (= id (:ynab.budget/id %)) %) budgets))

(pc/defresolver category-group-from-id [env {:keys [ynab.category-group/id
                                                    ynab.budget/category-groups]}]
  {::pc/input  #{:ynab.category-group/id
                 :ynab.budget/category-groups}
   ::pc/output [{:ynab.category-group/categories
                 [:ynab.category-group/id
                  :ynab.category/activity
                  :ynab.category/balance
                  :ynab.category/budgeted
                  :ynab.category/deleted
                  :ynab.category/goal-creation-month
                  :ynab.category/goal-percentage-complete
                  :ynab.category/goal-target
                  :ynab.category/goal-target-month
                  :ynab.category/goal-type
                  :ynab.category/hidden
                  :ynab.category/id
                  :ynab.category/name
                  :ynab.category/note
                  :ynab.category/original-category-group-id]}
                :ynab.category-group/deleted
                :ynab.category-group/hidden
                :ynab.category-group/id
                :ynab.category-group/name]}
  (some #(if (= id (:ynab.category-group/id %)) %) category-groups))

(pc/defresolver category-from-id [env {:keys [ynab.category/id
                                              ynab.budget/categories]}]
  {::pc/input  #{:ynab.category/id
                 :ynab.budget/categories}
   ::pc/output [:ynab.category-group/id
                :ynab.category/activity
                :ynab.category/balance
                :ynab.category/budgeted
                :ynab.category/deleted
                :ynab.category/goal-creation-month
                :ynab.category/goal-percentage-complete
                :ynab.category/goal-target
                :ynab.category/goal-target-month
                :ynab.category/goal-type
                :ynab.category/hidden
                :ynab.category/id
                :ynab.category/name
                :ynab.category/note
                :ynab.category/original-category-group-id]}
  (some #(if (= id (:ynab.category/id %)) %) categories))

(pc/defresolver account-from-id
  [env {:keys [ynab.account/id
               ynab.budget/accounts]}]
  {::pc/input  #{:ynab.account/id
                 :ynab.budget/accounts}
   ::pc/output [:ynab.account/balance
                :ynab.account/cleared-balance
                :ynab.account/closed
                :ynab.account/deleted
                :ynab.account/id
                :ynab.account/name
                :ynab.account/note
                :ynab.account/on-budget
                :ynab.account/transfer-payee-id
                :ynab.account/type
                :ynab.account/uncleared-balance]}
  (some #(if (= id (:ynab.account/id %)) %) accounts))

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
  [budgets
   budget-accounts
   budget-transactions
   budget-category-groups
   budget-categories
   budget-from-id
   category-group-from-id
   category-from-id
   account-from-id
   create-transaction])
