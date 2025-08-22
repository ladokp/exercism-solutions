(ns bank-account)

;; Function to open a new bank account.
;; It creates an atom with an initial balance of 0.
(defn open-account [] (atom 0))

;; Function to close an account.
;; It sets the atom's value to `nil`, indicating that the account is closed.
(defn close-account [account] (reset! account nil))

;; Function to get the current balance of the account.
;; It dereferences the atom to retrieve the balance value.
(defn get-balance [account] @account)

;; Function to update the balance of the account.
;; It takes an account and an amount, and updates the balance by adding the amount.
;; Uses `swap!` to apply the `+` function on the current value of the account atom.
(defn update-balance [account amount] (swap! account + amount))
