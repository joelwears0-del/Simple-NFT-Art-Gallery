;; NFT Art Gallery - Main Contract
;; A comprehensive NFT minting and marketplace platform for artists

;; Define the NFT trait
(define-non-fungible-token art-piece uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-token-not-found (err u104))
(define-constant err-already-listed (err u105))
(define-constant err-cannot-buy-own-token (err u106))
(define-constant err-invalid-price (err u107))
(define-constant err-unauthorized (err u108))

;; Data Variables
(define-data-var next-token-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%
(define-data-var total-minted uint u0)

;; Data Maps
(define-map token-metadata uint {
    title: (string-ascii 64),
    description: (string-ascii 256),
    image-uri: (string-ascii 256),
    artist: principal,
    minted-at: uint,
    category: (string-ascii 32)
})

(define-map marketplace-listings uint {
    seller: principal,
    price: uint,
    listed-at: uint
})

(define-map artist-profiles principal {
    name: (string-ascii 32),
    bio: (string-ascii 128),
    total-minted: uint,
    total-sales: uint,
    verified: bool
})

(define-map royalty-info uint {
    artist: principal,
    percentage: uint ;; basis points (100 = 1%)
})

;; Read-only functions
(define-read-only (get-token-uri (token-id uint))
    (match (map-get? token-metadata token-id)
        metadata (ok (some (get image-uri metadata)))
        (ok none)
    )
)

(define-read-only (get-token-metadata (token-id uint))
    (map-get? token-metadata token-id)
)

(define-read-only (get-token-owner (token-id uint))
    (ok (nft-get-owner? art-piece token-id))
)

(define-read-only (get-marketplace-listing (token-id uint))
    (map-get? marketplace-listings token-id)
)

(define-read-only (get-artist-profile (artist principal))
    (map-get? artist-profiles artist)
)

(define-read-only (get-next-token-id)
    (var-get next-token-id)
)

(define-read-only (get-total-minted)
    (var-get total-minted)
)

(define-read-only (get-platform-fee-percentage)
    (var-get platform-fee-percentage)
)

;; Public functions

;; Mint a new NFT
(define-public (mint-nft
    (title (string-ascii 64))
    (description (string-ascii 256))
    (image-uri (string-ascii 256))
    (category (string-ascii 32))
    (royalty-percentage uint))
    (let
        (
            (current-token-id (var-get next-token-id))
            (artist tx-sender)
        )
        ;; Validate royalty percentage (max 20%)
        (asserts! (<= royalty-percentage u2000) err-invalid-price)

        ;; Mint the NFT
        (try! (nft-mint? art-piece current-token-id artist))

        ;; Store metadata
        (map-set token-metadata current-token-id {
            title: title,
            description: description,
            image-uri: image-uri,
            artist: artist,
            minted-at: stacks-block-height,
            category: category
        })

        ;; Store royalty info
        (map-set royalty-info current-token-id {
            artist: artist,
            percentage: royalty-percentage
        })

        ;; Update artist profile
        (match (map-get? artist-profiles artist)
            profile (map-set artist-profiles artist
                (merge profile { total-minted: (+ (get total-minted profile) u1) }))
            (map-set artist-profiles artist {
                name: "",
                bio: "",
                total-minted: u1,
                total-sales: u0,
                verified: false
            })
        )

        ;; Update counters
        (var-set next-token-id (+ current-token-id u1))
        (var-set total-minted (+ (var-get total-minted) u1))

        (ok current-token-id)
    )
)

;; Update artist profile
(define-public (update-artist-profile
    (name (string-ascii 32))
    (bio (string-ascii 128)))
    (let ((current-profile (default-to
            { name: "", bio: "", total-minted: u0, total-sales: u0, verified: false }
            (map-get? artist-profiles tx-sender))))
        (map-set artist-profiles tx-sender
            (merge current-profile { name: name, bio: bio }))
        (ok true)
    )
)

;; List NFT for sale
(define-public (list-for-sale (token-id uint) (price uint))
    (let ((token-owner (unwrap! (nft-get-owner? art-piece token-id) err-token-not-found)))
        ;; Verify ownership
        (asserts! (is-eq token-owner tx-sender) err-not-token-owner)
        ;; Verify price is valid
        (asserts! (> price u0) err-invalid-price)
        ;; Verify token is not already listed
        (asserts! (is-none (map-get? marketplace-listings token-id)) err-already-listed)

        ;; Create listing
        (map-set marketplace-listings token-id {
            seller: tx-sender,
            price: price,
            listed-at: stacks-block-height
        })

        (ok true)
    )
)

;; Remove listing
(define-public (unlist-token (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found)))
        ;; Verify seller
        (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)

        ;; Remove listing
        (map-delete marketplace-listings token-id)
        (ok true)
    )
)

;; Buy NFT from marketplace
(define-public (buy-nft (token-id uint))
    (let
        (
            (listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
            (seller (get seller listing))
            (price (get price listing))
            (buyer tx-sender)
            (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
            (royalty-data (map-get? royalty-info token-id))
            (royalty-fee (match royalty-data
                data (/ (* price (get percentage data)) u10000)
                u0))
            (seller-amount (- (- price platform-fee) royalty-fee))
        )
        ;; Verify buyer is not seller
        (asserts! (not (is-eq buyer seller)) err-cannot-buy-own-token)

        ;; Transfer payment to seller
        (try! (stx-transfer? seller-amount buyer seller))

        ;; Transfer platform fee
        (try! (stx-transfer? platform-fee buyer contract-owner))

        ;; Transfer royalty to original artist if applicable
        (match royalty-data
            data (try! (stx-transfer? royalty-fee buyer (get artist data)))
            true)

        ;; Transfer NFT
        (try! (nft-transfer? art-piece token-id seller buyer))

        ;; Remove listing
        (map-delete marketplace-listings token-id)

        ;; Update artist sales count
        (match (map-get? artist-profiles seller)
            profile (map-set artist-profiles seller
                (merge profile { total-sales: (+ (get total-sales profile) u1) }))
            true)

        (ok true)
    )
)

;; Transfer NFT (removes from marketplace if listed)
(define-public (transfer-nft (token-id uint) (recipient principal))
    (let ((token-owner (unwrap! (nft-get-owner? art-piece token-id) err-token-not-found)))
        ;; Verify ownership
        (asserts! (is-eq token-owner tx-sender) err-not-token-owner)

        ;; Remove from marketplace if listed
        (map-delete marketplace-listings token-id)

        ;; Transfer NFT
        (try! (nft-transfer? art-piece token-id tx-sender recipient))
        (ok true)
    )
)

;; Admin functions
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-price) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

(define-public (verify-artist (artist principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (match (map-get? artist-profiles artist)
            profile (map-set artist-profiles artist
                (merge profile { verified: true }))
            (map-set artist-profiles artist {
                name: "",
                bio: "",
                total-minted: u0,
                total-sales: u0,
                verified: true
            }))
        (ok true)
    )
)
