(module svnwiki2html ()

(import scheme)
(cond-expand
 (chicken-4
  (use qwiki utils svnwiki-sxml sxml-transforms))
 ((or chicken-5 chicken-6)
  (import (chicken base)
          (chicken format)
          (chicken pathname)
          (chicken port)
          (chicken process-context)
          (chicken string))
  (import qwiki svnwiki-sxml sxml-transforms srfi-1 srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))

(define svnwiki2html-version "0.0.3")

(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define (read-wiki-data #!optional file)
  (with-output-to-string
    (lambda ()
      (write-content
       (if file
           (call-with-input-file file svnwiki->sxml)
           (svnwiki->sxml (current-input-port)))))))

(define (wiki->html #!key file title css menu)
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
                           "http://wiki.call-cc.org/chicken.css"))
                (type "text/css")))
       (link (@ (rel "icon")
                (href "http://call-cc.org/favicon.ico")
                (type "image/x-icon"))))
      ,(if menu
           `(div (@ (id "menu"))
                 (literal ,(read-wiki-data menu)))
           '())
      (div (@ (id "content"))
           (literal ,(read-wiki-data file)))))))

(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
#prog <options>

This program converts input in svnwiki format to HTML.  The HTML
data is printed to the standard output.

<options> are:

--css <CSS URI>
  URI to the CSS file to be linked from the HTML page.  If not
  provided, the default one used by the CHICKEN wiki will be used.

--menu <menu file>
  Path to the menu file to be added to the HTML page.  If not provided
  no menu will be added to the HTML page.  The menu file is expected
  to be in svnwiki syntax.

--title <page title>
  Text to be used as title for the HTML page.  If not provided
  the page will be generated with "CHICKEN Scheme" as title.

--version
  Print version and exit.

<wiki file>
  The wiki file to be converted to HTML.  If not provided, #prog
  will read from the standard input.

EOF
        ))
    (fprintf port msg)
    (when exit-code (exit exit-code))))


(let ((wiki-file #f)
      (css #f)
      (title #f)
      (menu-file #f))
  (let loop ((args (command-line-arguments)))
    (if (null? args)
        (print (wiki->html file: wiki-file
                           css: css
                           title: title
                           menu: menu-file))
        (let ((arg (car args)))
          (cond ((or (equal? arg "-h")
                     (equal? arg "--help"))
                 (usage 0))
                ((equal? arg "--version")
                 (print svnwiki2html-version)
                 (exit 0))
                ((equal? arg "--css")
                 (set! css (cadr args))
                 (loop (cddr args)))
                ((equal? arg "--title")
                 (set! title (cadr args))
                 (loop (cddr args)))
                ((equal? arg "--menu")
                 (set! menu-file (cadr args))
                 (loop (cddr args)))
                (else
                 (set! wiki-file arg)
                 (loop (cdr args))))))))

) ;; end module
