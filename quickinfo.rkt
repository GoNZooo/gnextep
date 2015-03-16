#lang racket

(define show-list '("Modern_Family"
                    "New_Girl"
                    "Game_of_Thrones"
                    "Elementary"
                    "Community"))

(require "fetch.rkt")

(module+ main
  (for-each output-show-data show-list))
