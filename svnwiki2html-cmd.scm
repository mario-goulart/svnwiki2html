(module svnwiki2html-cmd ()

(import scheme)
(cond-expand
 (chicken-4
  (use svnwiki2html))
 ((or chicken-5 chicken-6)
  (import (chicken base)
          (chicken format)
          (chicken pathname)
          (chicken process-context))
  (import svnwiki2html))
 (else
  (error "Unsupported CHICKEN version.")))

(define svnwiki2html-version "0.0.3")

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
        (if wiki-file
            (display
             (with-input-from-file wiki-file
               (lambda ()
                 (wiki->html-page css: css
                                  title: title
                                  menu: menu-file))))
            (usage 1))
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
