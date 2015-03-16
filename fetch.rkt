#lang racket

(provide output-show-data
         name->show-data)

(require net/url
         "api-urls.rkt")

(define qi-url (id->url "quickinfo"))

(define (name->show-data name)

  (define (get-raw-data)
    (let* ([request-url qi-url]
           [request-url (string-append request-url name)]
           [http-port (get-pure-port (string->url request-url))]
           [data (port->string http-port)])
      (close-input-port http-port)
      data))

  (define (extract-relevant-info [data (get-raw-data)])
    (define (data->id)
      (regexp-match #rx"Show ID@([0-9]*)" data))
    (define (data->name)
      (regexp-match #rx"Show Name@([0-9A-Za-z ]*)" data))
    (define (data->latest-ep)
      (regexp-match #rx"Latest Episode@(.*?)\n" data))
    (define (data->next-ep)
      (regexp-match #rx"Next Episode@(.*?)\n" data))

    (list (data->id)
          (data->name)
          (data->latest-ep)
          (data->next-ep)))

  (define (format-result result)
    
    (define (get-id)
      (second (first result)))
    (define (get-name)
      (second (second result)))
    (define (get-latest-ep)
      (string-split (second (third result)) "^"))
    (define (get-next-ep)
      (if (equal? (fourth result) #f)
        '("N/A" "N/A" "N/A")
        (string-split (second (fourth result)) "^")))

    (list (get-id)
          (get-name)
          (get-latest-ep)
          (get-next-ep)))

  (let ([extracted-data (extract-relevant-info)])
      (format-result extracted-data)))

(define (output-show-data name)
  
  (define (print-data data)
    
    (define (build-ep-string which)

      (define (get-index-func)
        (if (equal? which 'latest)
          third
          fourth))

      (define index-func (get-index-func))
      (let ([episode-number (first (index-func data))]
            [episode-name (second (index-func data))]
            [episode-date (third (index-func data))])
        (string-append episode-number " - " episode-name
                       " (" episode-date ")")))
    
    (printf "~a~n" (second data))
    (printf "Latest: ~a~nNext: ~a~n~n"
            (build-ep-string 'latest)
            (build-ep-string 'next)))
  (let ([show-data (name->show-data name)])
    (print-data show-data)))
