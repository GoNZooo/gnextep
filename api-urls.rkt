#lang racket

(provide id->url)

(define (id->url id)
  
  (define url-list
    '(("quickinfo" "http://services.tvrage.com/tools/quickinfo.php?show=")))

  (define (find-url [item-list url-list])
    (if (null? item-list)
      #f
      (let ([current-item (first item-list)])
        (if (equal? (first current-item) id)
          (second current-item)
          (find-url (rest item-list))))))

  (find-url))
