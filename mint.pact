(namespace "free")

(define-keyset "free.kai-mint-gov" (read-keyset "kai-mint-gov"))

(module kai-mint gov
    
    (defcap gov ()
    (enforce-guard (keyset-ref-guard "free.kai-mint-gov" ))
    )

    (defcap MINTPROCESS ()
    true
    )

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

(defun pay:bool (pay-data:object)
@doc "Create a new payment for minting"

    (let* (
        (payer:string (at "payer" pay-data))
        (payerGuard:guard (at "payerGuard" pay-data))
        (amount:decimal (at "amount" pay-data))
        (fungible:module{fungible-v2} (at "fungible" pay-data))
        (date:time (curr-time))
        (id:string (hash {'payer:payer, 'amount:amount, 'date:date, 'guard:payerGuard}))
    )
    ; we need to add the contract to repl for zusd, replace second coin
    (util.guards.enforce-or (= coin fungible)(= coin fungible))
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
(defpact run-payment:bool (pay-data:object mint-data:object)
 (step-with-rollback
    ;; Step 1: Send a payment to escrow
    (pay pay-data)
    "yay"
    )
    (step 
        ;; Step 1: Mint the NFT
"yay"
        )
 )

; Mint Functions

; Lets modify this further so it works for us
;  (defun create-marmalade-token:string
;      (
;        uri:string
;        precision:integer 
;        collection:string
;        marmToken:integer
;        policies:[module{kip.token-policy-v2}]
;        id:string
;      )
;      @doc "Requires mint cap. Creates the token on marmalade using the supplied data"
;      ; We can craft a new cap for this, just pass true for now
;    (with-capability (MINTPROCESS)
;      (with-read payments id
;      {
;        "id":= id
;        , "minted":= minted
;      }
;      (enforce (= minted false)
;      "Can't mint this token more than once"
;      )
;          (let*
;          (
;            (guard:guard (at 'creatorGuard (read collections collection ['creatorGuard ])))
;            (mintto:guard (at "guard" (coin.details account)))
;            (token-id:string (create-token-id {'precision:precision, 'policies: policies, 'uri:uri} guard))        
;          )
;          ; This is required to create an actual NFT Token
;          (create-token
;            token-id
;            precision
;            uri
;            policies
;            guard
;          )
;          ; This is where the actual NFT is minted on the ledger.
;          (marmalade-v2.ledger.mint
;            token-id
;            account
;            mintto
;            1.0
;          )
  
;          (update payments id
;            { "minted": true }          
;          )
  
;          ; Should we create this table structure? 
;          (insert nft-table token-id
;            {
;              "id": token-id,
;              "owner": account
;            }
;          )
;          token-id
;        )
;      )
;     )
;    )

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

)