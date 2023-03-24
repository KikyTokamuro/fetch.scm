#!/usr/bin/guile \
--no-auto-compile -e main -s
!#

;; System information fetcher written in GNU Guile Scheme
;; https://github.com/KikyTokamuro/fetch.scm

;; MIT License

;; Copyright (c) 2021-2023 Kiky Tokamuro (Daniil Archangelsky) <kiky.tokamuro@yandex.ru>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(use-modules (ice-9 format)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 popen)
	     (ice-9 getopt-long))

(define (run-cmd cmd)
  "Run command and return output"
  (let* ((port (open-input-pipe cmd))
	 (str  (read-line port)))
    (close-pipe port)
    str))

(define* (split-path #:optional (path (getenv "PATH")) (separator #\:))
  "Split path string"
  (string-tokenize path (char-set-complement (char-set separator))))

(define (which program)
  "Return program path or #f"
  (search-path (split-path) program))

(define (get-kernel)
  "Return Kernel version"
  (format #f "~a ~a"
	  (utsname:sysname (uname))
	  (utsname:release (uname))))

(define (get-arch)
  "Return the architecture of the computer"
  (utsname:machine (uname)))

(define (get-hostname)
  "Return hostname"
  (utsname:nodename (uname)))

(define (get-pwd)
  "Return pwd string"
  (getpwuid (getuid)))

(define (get-username)
  "Return username"
  (passwd:name (get-pwd)))

(define (get-shell)
  "Return current shell"
  (passwd:shell (get-pwd)))

(define (get-proc-uptime)
  "Return uptime in seconds"
  (inexact->exact
   (round (with-input-from-file "/proc/uptime" read))))

(define (pretty-seconds uptime)
  "Return pretty time from seconds"
  (let* ((min (round (/ (modulo uptime 3600) 60)))
	 (hour (modulo (round (/ uptime 3600)) 24))
	 (day (round (/ uptime 86400))))
    (format #f "~a days ~2,'0d:~2,'0d" day hour min)))

(define (get-uptime)
  "Return uptime"
  (cond ((file-exists? "/proc/uptime") (pretty-seconds (get-proc-uptime)))
	((which "uptime") (run-cmd "uptime -p"))
	(else "unknown")))

(define (os-release-name)
  "Return ID from os-release file"
  (let ((os (with-input-from-file "/etc/os-release" read-string)))
    (match:substring (string-match "PRETTY_NAME=\"([A-Za-z0-9(). ]+)\"" os) 1)))

(define (get-distro)
  "Return distro name"
  (cond ((file-exists? "/etc/os-release") (os-release-name))
	((which "lsb_release") (string-trim-both (run-cmd "lsb_release -sd") #\"))
	((which "uname") (run-cmd "uname -o"))
	(else "unknown")))

(define (cpuinfo-model)
  "Return model from cpuinfo file"
  (let ((cpuinfo (with-input-from-file "/proc/cpuinfo" read-string)))
    (match:substring (string-match "model name\t: ([A-Za-z0-9 ()-@]+)\n" cpuinfo) 1)))

(define (get-cpu)
  "Return CPU"
  (cond ((file-exists? "/proc/cpuinfo") (cpuinfo-model))
	(else "unknown")))

(define (hw-model)
  (let ((name (string-trim-right
	       (with-input-from-file "/sys/devices/virtual/dmi/id/product_name" read-string)))
	(version (string-trim-right
		  (with-input-from-file "/sys/devices/virtual/dmi/id/product_version" read-string))))
    (string-append name " " version)))

(define (get-model)
  "Return hardware model"
  (cond ((and (file-exists? "/sys/devices/virtual/dmi/id/product_name")
	      (file-exists? "/sys/devices/virtual/dmi/id/product_version"))
	 (let ((model (hw-model)))
	   (if (string-contains model "To Be Filled By O.E.M.") "unknown" model)))
	(else "unknown")))

(define (get-desktop)
  "Return desktop info"
  (define xdg-current-desktop (getenv "XDG_CURRENT_DESKTOP"))
  (define desktop-session (getenv "DESKTOP_SESSION"))
  (cond (xdg-current-desktop (string-downcase xdg-current-desktop))
	(desktop-session (string-downcase desktop-session))
	(else "unknown")))

(define (green text)
  "Coloring text to green color"
  (format #f "\x1b[32m~a\x1b[0m" text))

(define (print-info)
  "Print system info"
  (for-each (lambda (x)
	      (let ((info-element (eval (list (symbol-append 'get- x)) (interaction-environment))))
		(if (not (string= info-element "unknown"))
		    (format #t "~18a ~a\n" (green (symbol->string x)) info-element))))
	    '(username hostname distro desktop arch model cpu kernel uptime shell)))

(define help-message "fetch.scm - system information fetcher\n
fetch.scm [options]
  -v, --version    Display version
  -h, --help       Display this help")

(define version-message "fetch.scm v0.1.6")

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (if (or version-wanted help-wanted)
        (begin
          (if version-wanted (format #t "~a\n" version-message))
          (if help-wanted (format #t "~a\n" help-message)))
        (print-info))))
