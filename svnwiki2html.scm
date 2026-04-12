(module svnwiki2html

(wiki->html wiki->html-page)

(import scheme)
(cond-expand
 ((or chicken-5 chicken-6)
  (import (chicken base)
          (chicken port))
  (import qwiki svnwiki-sxml sxml-transforms))
 (else
  (error "Unsupported CHICKEN version.")))


(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define (wiki->html #!optional (port (current-input-port)))
  ;; Write HTML in SXML format to current-output-port
  (write-content
   (with-input-from-port port
     (lambda ()
       (svnwiki->sxml port)))))

(define (wiki->html-page #!key title css menu (port (current-input-port)))
  ;; Read svnwiki text from `port' and write HTML as a string to
  ;; current-output-port
  (sxml->html
   `((literal "<!DOCTYPE html>")
     (html
      (head
       (title ,(or title "CHICKEN Scheme"))
       (meta (@ (http-equiv "Content-Type")
                (content "application/xhtml+xml; charset=utf-8")))
       (link (@ (rel "stylesheet")
                (href ,(if css
                           css
                           "//wiki.call-cc.org/chicken.css"))
                (type "text/css")))
       (link (@ (rel "icon")
                (href "//call-cc.org/favicon.ico")
                (type "image/x-icon"))))
      ,(if menu
           `(div (@ (id "menu"))
                 (literal ,(with-output-to-string
                             (lambda ()
                               (with-input-from-file menu wiki->html)))))
           '())
      (div (@ (id "content"))
           (literal ,(with-output-to-string
                       (lambda ()
                         (wiki->html port)))))))))

) ;; end module
