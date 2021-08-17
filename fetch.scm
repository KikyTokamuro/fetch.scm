#!/usr/bin/guile
!#

;; MIT License

;; Copyright (c) 2021 Kiky Tokamuro (Daniil Archangelsky)

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
	     (ice-9 popen))

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

(define (get-uptime-seconds)
  "Return uptime in seconds"
  (inexact->exact
   (round (with-input-from-file "/proc/uptime" read))))

(define (pretty-uptime uptime)
  "Return pretty time from seconds"
  (let* ((min (round (/ (modulo uptime 3600) 60)))
	 (hour (modulo (round (/ uptime 3600)) 24))
	 (day (round (/ uptime 86400))))
    (format #f "~a days ~2,'0d:~2,'0d" day hour min)))

(define (get-uptime)
  "Return uptime"
  (pretty-uptime (get-uptime-seconds)))

(define* (split-path #:optional (path (getenv "PATH")) (separator #\:))
  "Split path string"
  (string-tokenize path (char-set-complement (char-set separator))))

(define (which program)
  "Return program path or #f"
  (search-path (split-path) program))

(define (os-release-name path)
  "Return ID from os-release file"
  (let ((os (with-input-from-file path read-string)))
    (match:substring (string-match "PRETTY_NAME=\"([A-Za-z ]+)\"" os) 1)))

(define (run-cmd cmd)
  "Run command and return output"
  (let* ((port (open-input-pipe cmd))
	 (str  (read-line port)))
    (close-pipe port)
    str))

(define (get-distro)
  "Return distro name"
  (cond ((file-exists? "/etc/os-release") (os-release-name "/etc/os-release"))
	((file-exists? "/usr/lib/os-release") (os-release-name "/usr/lib/os-release"))
	((which "lsb_release") (string-trim-both (run-cmd "lsb_release -sd") #\"))
	(else (run-cmd "uname -o"))))

(define (green text)
  "Coloring text to green color"
  (format #f "\x1b[32m~a\x1b[0m" text))

(define (print-info)
  "Print system info"
  (let ((username (get-username))
	(hostname (get-hostname))
	(distro (get-distro))
	(arch (get-arch))
	(kernel (get-kernel))
	(uptime (get-uptime))
	(shell (get-shell)))
    (format #t "~18a -> ~a\n" (green "username") username)
    (format #t "~18a -> ~a\n" (green "hostname") hostname)
    (format #t "~18a -> ~a\n" (green "distro") distro)
    (format #t "~18a -> ~a\n" (green "arch") arch)
    (format #t "~18a -> ~a\n" (green "kernel") kernel)
    (format #t "~18a -> ~a\n" (green "uptime") uptime)
    (format #t "~18a -> ~a\n" (green "shell") shell)))

(print-info)
