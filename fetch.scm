#!/usr/bin/guile
!#

(use-modules (ice-9 format)
	     (ice-9 rdelim)
	     (ice-9 regex))

(define (get-kernel)
  (format #f "~a ~a"
	  (utsname:sysname (uname))
	  (utsname:release (uname))))

(define (get-arch)
  (utsname:machine (uname)))

(define (get-hostname)
  (utsname:nodename (uname)))

(define (get-pwd)
  (getpwuid (getuid)))

(define (get-username)
  (passwd:name (get-pwd)))

(define (get-shell)
  (passwd:shell (get-pwd)))

(define (get-uptime-seconds)
  (inexact->exact
   (round (with-input-from-file "/proc/uptime" read))))

(define (pretty-uptime uptime)
  (let* ((min (round (/ (modulo uptime 3600) 60)))
	 (hour (modulo (round (/ uptime 3600)) 24))
	 (day (round (/ uptime 86400))))
    (format #f "~a days ~2,'0d:~2,'0d" day hour min)))

(define (get-uptime)
  (pretty-uptime (get-uptime-seconds)))

(define (get-distro)
  (let ((os (with-input-from-file "/etc/os-release" read-string)))
    (match:substring (string-match "ID=([A-Za-z]+)" os) 1)))

(define (green text)
  (format #f "\x1b[32m~a\x1b[0m" text))

(define (print-info)
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
