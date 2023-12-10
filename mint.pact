(namespace "free")

(define-keyset "free.kai-mint-gov" (read-keyset "kai-mint-gov"))
(define-keyset "free.kai-mint-bank" (read-keyset "kai-mint-bank"))

(module kai-mint GOV
    
    (defcap GOV ()
    (enforce-guard (keyset-ref-guard "free.kai-mint-gov" ))
    )

    (defcap MINTPROCESS ()
    true
    )

    (use marmalade-v2.ledger)

; Lets revisit to make sure this can handle off chain onboarding
; of payments with stripe or other processors.  
    (defschema pay-schema
        id:string
        payer:string
        payerGuard:guard
        amount:decimal
        fungible:module{fungible-v2}
        minted:bool
    )
(deftable payments:{pay-schema})


; Sale Process 

(defcap PAYCAP:bool
    (payment-id:string payer:string amount:decimal sale-id:string)
    @doc "Wrapper cap/event of SALE for event processing"
    @event
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (SENDPAY payment-id payer amount))
  )

  (defcap SENDPAY:bool
    (payment-id:string payer:string amount:decimal)
    @doc "Managed cap for PAYER to store mint funds."
    @managed
     ; Do we need an enforce in here?  I think we do
    (compose-capability (DEBIT payer))
    (compose-capability (CREDIT (escrow)))
  )

(defun account-guard:guard (account:string)
  @doc "Retrieves the guard associated with the given account."
  (let* ((details (coin.details account))
         (guard (at "guard" details)))
    guard
  )
)

(defun pay:bool (pay-data:object)
@doc "Create a new payment for minting"

    (let* (
        (payer:string (at "payer" pay-data))
        (payerGuard:guard (at "payerGuard" pay-data))
        (amount:decimal (at "amount" pay-data))
        (fungible:module{fungible-v2} (at "fungible" pay-data))
        (date:time (curr-time))
        (id:string (hash {'payer:payer, 'amount:amount, 'date:date, 'guard:payerGuard}))
        (bank:string (get-bank))
    )
    (enforce (= coin fungible)false)
    (fungible::transfer payer bank amount)
    ; we need to add the contract to repl for zusd, replace second coin
    (insert payments id {
        'id: id,
        'payer: payer,
        'payerGuard: payerGuard,
        'amount: amount,
        'fungible: fungible,
        'minted: false
    } )
    )

true
)

; We will need to compartmentalize the information in here and figure out a clean
; method to take a payment, record the successful state for minting, and handle
; return or release of payments from the escrow.  Placeholders for now
;  (defpact run-payment:bool (pay-data:object mint-data:object)
;   (step-with-rollback
;      ;; Step 1: Send a payment to escrow
;      (pay pay-data)
;      "yay"
;      )
;      (step 
;          ;; Step 1: Mint the NFT
;  "yay"
;          )
;   )

(defpact run-payment:bool (pay-data:object)
@doc "Handles the payment and minting process in sequential steps."
(step-with-rollback
    (let* (
        (payment-id:string (pay pay-data))
        (payment-record (get-payment-info payment-id)) 
        (valid-payment (validate-payment payment-record)) 
    )
    (enforce valid-payment "Payment validation failed.")
    (with-capability (SALE payment-id (at "payer" pay-data) (at "amount" pay-data) (pact-id))
      (transfer-to-escrow payment-record)) ; Function to handle transfer to escrow
    payment-id ; Continue with payment-id for next step 
    )
    (let ((payment-record (get-payment-info (resume))))
      (refund-payment payment-record)) ; Function to handle refund in case of rollback
  )
(step 
    (let* (
        (payment-id:string (resume))
        (mint-data:object (read-msg "mint-data"))
        (token-id:string (at "token-id" mint-data))
        (uri:string (at "uri" mint-data))
        (policies (at "policies" mint-data))
        (pay-guard:guard (at "pay-guard" mint-data)) 
        (precision:integer 0)
        (mint-to-account:string (at "mint-to-account" mint-data))
        (mint-to-guard:guard (at "guard" (coin.details mint-to-account)))
        (new-token-id:string (create-token-id {'precision:precision, 'policies: policies, 'uri:uri} pay-guard))        
        (mint-success:bool (create-marmalade-token mint-data payment-id)) 
    )
    (enforce mint-success "Minting of NFT failed.")
    (release-funds-from-escrow payment-id) ; Function to handle fund release from escrow
    "Minting successful"
    )
)
)

; Mint Functions

; Lets modify this further so it works for us
(defun create-marmalade-token:string
    (
     mint-data:object
     id:string
    )
    @doc "Requires mint cap. Creates the token on marmalade using the supplied data"
    ; We can craft a new cap for this, just pass true for now
  (with-capability (MINTPROCESS)
    (with-read payments id
    {
      "id":= id
      , "minted":= minted
    }
    (enforce (= minted false)
    "Can't mint this token more than once"
    )
        (let*
        (
          (token-id:string (at "token-id" mint-data))
          (uri:string (at "uri" mint-data))
          (policies (at "policies" mint-data))
          (guard:guard (at "pay-guard" mint-data))
          (precision 0)
          (mint-to-account:string (at "mint-to-account" mint-data))
          (mint-to-guard:guard (at "guard" (coin.details mint-to-account)))
          (token-id:string (create-token-id {'precision:precision, 'policies: policies, 'uri:uri} guard))        
        )
        ; This is required to create an actual NFT Token
        (create-token
          token-id
          precision
          uri
          policies
          guard
        )
        ; This is where the actual NFT is minted on the ledger.
        (mint
          token-id
          mint-to-account
          mint-to-guard
          1.0
        )
  
        (update payments id
          { "minted": true }          
        )
  
        ; Should we create this table structure? 
        ;  (insert nft-table token-id
        ;    {
        ;      "id": token-id,
        ;      "owner": account
        ;    }
        ;  )
        token-id
      )
    )
   )
  )

; Utility functions

(defun get-payment-info:object{pay-schema} (payment-id:string)
    @doc "Retrieves payment information based on the payment ID."
    (read payments payment-id)
)

(defun validate-payment:bool (payment-record:object{pay-schema})
    @doc "Validates the payment details."
    (and (not (= payment-record {})) ; Check if payment record is not empty
         (not (at 'minted payment-record))) ; Ensure the payment has not been used for minting yet
)


(defun transfer-to-escrow:bool (payment-record:object{pay-schema})
    @doc "Transfers funds to the escrow account."
    
    ; (transfer payer (escrow) amount)
    ; Implement logic to transfer funds to the escrow account
    true ; Placeholder return value
)

(defun refund-payment:bool (payment-record:object{pay-schema})
    @doc "Handles refund in case of rollback."
    ; Implement our logic to refund payment
    true ; Placeholder return value
)

(defun release-funds-from-escrow:bool (payment-id:string)
    @doc "Handles fund release from escrow upon successful minting."
    ; Implement our logic to release funds from escrow
    true ; Placeholder return value
)


; Escrow functions 

(defcap ESCROW ()
true)

(defun escrow-guard:guard ()
@doc "Return the guard of the escrow account"
(create-capability-pact-guard (ESCROW)))

(defun escrow:string ()
@doc "Return the escrow account"
(create-principal (escrow-guard)))

 (defun curr-time:time ()
 @doc "Returns current chain's block-time"
 (at 'block-time (chain-data))
 )


; Bank Info

(defschema bank-info
    @doc "Stores string values"
    value:string
  )
  (deftable bankInfo:{bank-info})

  (defun update-bank (bankId:string value:string)
    @doc "Updates the account for the bank"

    (with-capability (GOV)
      (write bankInfo bankId
        { "value": value }
      )
    )
  )

  (defun get-bank-value:string (bankId:string)
    @doc "Gets the value with the provided id"
    (at "value" (read bankInfo bankId ["value"]))
  )

  (defconst BANK_ACCOUNT:string "BANK")
  
  (defun get-bank:string ()
    (get-bank-value BANK_ACCOUNT)
  )

  (defun get ()
  (select payments (constantly true)))

)
(if (read-msg "upgrade")
"Upgrade Complete"
[
(create-table payments)
(create-table bankInfo)
])