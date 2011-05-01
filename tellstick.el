;;; tellstick.el -- controlling tellstick controllers

;; Copyright (C) 2010 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: home automation

;; This file is not part of GNU Emacs.

;; tellstick.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; tellstick.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage example:
;;
;; (tellstick-send
;;   (tellstick-make-command tellstick-room-code 10 'on nil))
;;
;; This switches unit 10 on, without using dimming.
;;
;; To make a device learn that it's device 10 in room code 2342, say
;;
;; (tellstick-learn 2342 10)
;;
;; after putting the device in learning state.

;;; Code:

(require 'cl)

(defvar tellstick-room-code 31132
  "The code you're using for your units.")

(defvar tellstick-non-dimmers '(1 3 14 15 16))

;;; Somewhat bogus semaphores.

(defvar tellstick-semaphore '(nil))

(defun tellstick-get-semaphore (semaphore)
  "Wait until SEMAPHORE is released."
  (while (/= (length (nconc (symbol-value semaphore) (list nil))) 2)
    (sleep-for 0.1)))

(defun tellstick-release-semaphore (semaphore)
  "Release SEMAPHORE."
  (setcdr (symbol-value semaphore) nil))

(defmacro tellstick-with-semaphore (&rest forms)
  `(unwind-protect
       (progn
	 (tellstick-get-semaphore 'tellstick-semaphore)
	 ,@forms)
     (tellstick-release-semaphore 'tellstick-semaphore)))

(put 'tellstick-with-semaphore 'lisp-indent-function 0)
(put 'tellstick-with-semaphore 'edebug-form-spec '(body))

(defun tellstick-make-command (house unit command dim)
  (unless (fboundp 'insert-byte)
    (defun insert-byte (byte length)
      (insert byte)))
  (concat
   (with-temp-buffer
     (insert "R")
     (insert-byte (if (eq command 'learn)
		      2 5)
		  1)
     (insert "T")
     (insert-byte 127 1)
     (insert-byte 255 1)
     (insert-byte 24 1)
     (insert-byte 1 1)
     (insert-byte (if dim 147 132) 1)
     (buffer-string))
   (tellstick-encode-command
    (tellstick-format-command house unit command dim))
   "+"))

(defun tellstick-learn (house unit)
  ;; Apparently you need to send the learning code about five times
  ;; for the device to pick it up.
  (dotimes (i 5)
    (tellstick-send
     (tellstick-make-command house unit 'learn nil))
    (sit-for 0.2)))

(defun tellstick-format-command (house unit command dim)
  ;; There can be only 16 units per puny house.  If we get a unit code
  ;; higher than 16, then continue on to the next house code.
  (when (> unit 16)
    (setq house (+ house (/ unit 16))
	  unit (mod unit 16)))
  (concat
   (tellstick-double-binarify house 26)
   "01"
   (cond
    (dim
     "00")
    ((eq command 'off)
     "01")
    (t
     "10"))
   (tellstick-double-binarify (1- unit) 4)
   (if dim
       (tellstick-double-binarify dim 4)
     "")
   "0"))

(defun tellstick-encode-command (string)
  (with-temp-buffer
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary))
      (let ((i 0)
	    (result "")
	    (acc 9))
	(while (< i (length string))
	  (let ((char (aref string i)))
	    (setq acc (logior (lsh acc 4)
			      (if (equal char ?1)
				  8
				10)))
	    (when (zerop (mod i 2))
	      (insert-byte acc 1)
	      (setq acc 0))
	    (incf i)))
	(buffer-string)))))

(defun tellstick-binarify (number length)
  (let ((result nil))
    (while (plusp length)
      (decf length)
      (push (if (plusp (logand number 1))
		"1"
	      "0")
	    result)
      (setq number (lsh number -1)))
    (let ((nresult ""))
      (dolist (elem (reverse result))
	(setq nresult
	      (concat nresult elem)))
      nresult)))

(defun tellstick-double-binarify (number length)
  (let ((result nil))
    (while (plusp length)
      (decf length)
      (if (plusp (logand number 1))
	  (progn
	    (push "0" result)
	    (push "1" result))
	(push "1" result)
	(push "0" result))
      (setq number (lsh number -1)))
    (let ((nresult ""))
      (dolist (elem result)
	(setq nresult
	      (concat nresult elem)))
      nresult)))

(defun tellstick-send (&rest commands)
  (tellstick-with-semaphore
    (if (fboundp 'make-serial-process)
	(apply #'tellstick-send-serial commands)
      (apply #'tellstick-send-cu commands))))

(defun tellstick-send-cu (&rest commands)
  (with-temp-buffer
    (let ((process (start-process
		    "tellstick"
		    (current-buffer)
		    "cu" "-l" "/dev/tellstick" "-s" "4800" "dir")))
      (dolist (command commands)
	(process-send-string process command)
	(process-send-string process "\r\n")
	(while (not (save-excursion
		      (goto-char (point-min))
		      (re-search-forward "^\\+.*\r" nil t)))
	  (accept-process-output process 0 100)
	  ;;(message "%s" (buffer-string))
	  )
	(erase-buffer))
      (process-send-string process "\r~.\r")
      (prog1
	  (buffer-string)
	(delete-process process)
	(when (file-exists-p "/var/lock/LCK..tellstick")
	  (delete-file "/var/lock/LCK..tellstick"))))))

(defun tellstick-send-serial (&rest commands)
  (with-temp-buffer
    (let ((process (make-serial-process
		    :port "/dev/tellstick"
		    :speed 4800
		    :coding 'no-conversion
		    :buffer (current-buffer)))
	  result)
      (dolist (command commands)
	(process-send-string process command)
	(while (not (save-excursion
		      (goto-char (point-min))
		      (search-forward "\r" nil t)))
	  (accept-process-output process 0 100)
	  ;;(message "Got: %s" (buffer-string))
	  )
	(setq result (buffer-string))
	(erase-buffer))
      (delete-process process)
      result)))

(defun tellstick-switch-id (id action &optional dim)
  (tellstick-send
   (tellstick-make-command
    tellstick-room-code id action
    (and (eq action 'on)
	 ;; If it's a dimmer, we have to send the signal
	 ;; strength.
	 (not (member id tellstick-non-dimmers))
	 15))))

(defun tellstick-switch-room (rooms action)
  (when (eq action :off)
    (setq action 'off))
  (when (eq action :on)
    (setq action 'on))
  (let ((strings nil))
    (when (atom rooms)
      (setq rooms (list rooms)))
    (dolist (room rooms)
      (dolist (id (cond
		   ((eq room 'tv)
		    '(10 9 17))
		   ((eq room 'kitchen)
		    '(3 1))
		   ((eq room 'hall)
		    '(11 13))
		   ((eq room 'office)
		    '(14 15 16))
		   ((or (eq room 'living)
			(eq room :living))
		    '(4 5 6 8))
		   ((eq room 'bedroom)
		    '(2 12))))
	(push (tellstick-make-command
	       tellstick-room-code id action
	       (and (eq action 'on)
		    ;; If it's a dimmer, we have to send the signal
		    ;; strength.
		    (not (member id tellstick-non-dimmers))
		    15))
	      strings)))
    (apply #'tellstick-send strings)))

(defun tellstick-switch (action)
  (cond
   ((or (eq action 1)
	(eq action :on))
    (setq action 'on))
   ((or (eq action 0)
	(eq action :off))
    (setq action 'off)))
  (tellstick-switch-room '(tv living bedroom kitchen) action))

(defun tellstick-server ()
  (interactive)
  (let ((system (car (split-string (system-name) "[.]"))))
    (setq server-use-tcp t
	  server-host (system-name)
	  server-name system)
    (server-start)))

(provide 'tellstick)

;;; tellstick.el ends here
