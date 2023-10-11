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
;; (tellstick-send (tellstick-make-command tellstick-room-code 10 'on nil))
;;
;; This switches unit 10 on, without using dimming.
;;
;; To make a device learn that it's device 10 in room code 2342, say
;;
;; (tellstick-learn tellstick-room-code 10)
;;
;; after putting the device in learning state.

;;; Code:

(require 'eval-server)

(defvar tellstick-room-code 1
  "The code you're using for your units.")

(defvar tellstick-actions nil
  "Actions to be evaled when a command is read.")

(defvar tellstick-non-dimmers nil)

(defvar tellstick-dimmers nil)

(defvar tellstick-model 'uno
  "The model plugged in.
Valid values are `uno' (the classic stick) and `duo'.")

(defvar tellstick-room-ids nil
  "What switches are in which rooms.
This is a alist on the form
 \((tv 34 55)
   (bedroom 56 67))")

(defvar tellstick-central-server nil
  "The server that receives all commands and decides what to do about it.")

;;; Somewhat bogus semaphores.

(defvar tellstick-semaphore '(nil))

(defun tellstick-get-semaphore (semaphore)
  "Wait until SEMAPHORE is released."
  (while (/= (length (nconc (symbol-value semaphore) (list nil))) 2)
    (message "Waiting for semaphore")
    (with-current-buffer (get-buffer "*tellstick*")
      (message "Wait: %s" (buffer-string)))
    (accept-process-output nil 0 100)))

(defun tellstick-release-semaphore (semaphore)
  "Release SEMAPHORE."
  (setcdr (symbol-value semaphore) nil))

(defmacro tellstick-with-semaphore (&rest forms)
  `(unwind-protect
       (progn
	 (condition-case var
	     (progn
	       (tellstick-get-semaphore 'tellstick-semaphore)
	       ,@forms)
	   (quit (debug))))
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
     (tellstick-make-command house unit 'learn nil))))

(defun tellstick-format-command (house unit command dim)
  ;; There can be only 16 units per puny house.  If we get a unit code
  ;; higher than 16, then continue on to the next house code.
  (when (> unit 16)
    ;; The new Telldus switches needs certain bits blank which means
    ;; that only every 16 house code is valid.
    (setq house (+ house (* (/ unit 16) 16))
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
	    (setq acc (logior (ash acc 4)
			      (if (equal char ?1)
				  8
				10)))
	    (when (zerop (mod i 2))
	      (insert-byte acc 1)
	      (setq acc 0))
	    (cl-incf i)))
	(buffer-string)))))

(defun tellstick-binarify (number length)
  (let ((result nil))
    (while (cl-plusp length)
      (cl-decf length)
      (push (if (cl-plusp (logand number 1))
		"1"
	      "0")
	    result)
      (setq number (ash number -1)))
    (let ((nresult ""))
      (dolist (elem (reverse result))
	(setq nresult
	      (concat nresult elem)))
      nresult)))

(defun tellstick-double-binarify (number length)
  (let ((result nil))
    (while (cl-plusp length)
      (cl-decf length)
      (if (cl-plusp (logand number 1))
	  (progn
	    (push "0" result)
	    (push "1" result))
	(push "1" result)
	(push "0" result))
      (setq number (ash number -1)))
    (let ((nresult ""))
      (dolist (elem result)
	(setq nresult
	      (concat nresult elem)))
      nresult)))

(defvar tellstick-process nil)

(defun tellstick-send (&rest commands)
  (when (= (length tellstick-semaphore) 1)
    (tellstick-with-semaphore
      (if tellstick-process
	  (tellstick-send-process tellstick-process commands)
	(tellstick-send-serial commands)))))

(defun tellstick-send-serial (commands)
  (with-temp-buffer
    (let ((process (make-serial-process
		    :port "/dev/tellstick"
		    :speed (if (eq tellstick-model 'duo)
			       9600
			     4800)
		    :coding 'no-conversion
		    :buffer (current-buffer)))
	  result)
      (tellstick-send-process process commands)
      (delete-process process)
      result)))

(defun tellstick-send-process (process commands)
  (with-current-buffer (process-buffer process)
    (dolist (command commands)
      (let ((times 100))
	(goto-char (point-min))
	(while (search-forward "+T\r\n" nil t)
	  (delete-region (match-beginning 0) (match-end 0)))
	(process-send-string process command)
	(while (and (not (save-excursion
			   (goto-char (point-min))
			   (search-forward "+T\r\n" nil t)))
		    (> (cl-decf times) 0))
	  (accept-process-output process 0 100)
	  (redisplay t))
	(when (zerop times)
	  (message "Bailed after sending command"))))
    (buffer-string)))

(defun tellstick-start-reading ()
  (with-current-buffer (get-buffer-create "*tellstick*")
    (buffer-disable-undo)
    (erase-buffer)
    (setq tellstick-process
	  (make-serial-process
	   :port "/dev/tellstick"
	   :speed (if (eq tellstick-model 'duo)
		      9600
		    4800)
	   :coding 'no-conversion
	   :buffer (current-buffer)))
    (set-process-filter tellstick-process 'tellstick-filter)))
    
(defun tellstick-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "^\\+W.*\r\n" nil t)
      (let ((string (match-string 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(tellstick-handle-input string)))))

(defun tellstick-handle-input (string)
  (setq string (replace-regexp-in-string "[\r\n]" "" string))
  (message "Got: '%s'" string)
  (when (and (string-match "arctech.*data:0x\\([0-9A-F]+\\)" string)
	     ;; These are spurious data points that should be filtered
	     ;; out.
	     (not (string-match "data:0x0;$" string)))
    (tellstick-queue-action
     `(eval-at "lights" "rocket-sam"
	       8701 (tellstick-receive-command ,string)))))

(defun tellstick-transmit (data)
  (message "Transmitting %s" data)
  (tellstick-send data))

(defvar tellstick-action-queue nil)

(defun tellstick-queue-action (action)
  (push (cons (float-time) action) tellstick-action-queue))

(defun tellstick-queue-runner ()
  (let ((action (car tellstick-action-queue)))
    (when (and action
	       (> (- (float-time) (car action)) 0.2))
      (setq tellstick-action-queue nil)
      (message "Executing %S" action)
      (apply 'funcall (cdr action)))))

(defun tellstick-central-queue ()
  (when tellstick-action-queue
    (let ((action (car (last tellstick-action-queue))))
      (setq tellstick-action-queue nil)
      (message "Executing %S" action)
      (apply 'funcall (cdr action)))))

(defvar tellstick-last-input 0)
(defvar tellstick-previous-input nil)

(defun tellstick-receive-command (string)
  (message "Central got: '%s'" string)
  (when (and (string-match "arctech.*data:0x\\([0-9A-F]+\\)" string)
	     (or (not (equal tellstick-previous-input (match-string 1 string)))
		 (> (- (float-time) tellstick-last-input) 1))
	     (not (equal (match-string 1 string) "0")))
    (setq tellstick-last-input (float-time)
	  tellstick-previous-input (match-string 1 string))
    (tellstick-queue-action
     `(tellstick-execute-input ,(match-string 1 string)))))
  
(defun tellstick-execute-input (data)
  (let ((action (cdr (assoc data tellstick-actions))))
    (cond
     ((not action)
      (message "No action for command '%s'" data))
     ((and (consp (car action))
	   (consp (cdar action)))
      (message "Executing %s" (car action))
      (eval (car action)))
     (t
      (let ((command (car action))
	    codes)
	(if (consp command)
	    ;; On the form '((on . room-bedroom) (off . apartment)).
	    (dolist (elem action)
	      (setq codes
		    (append codes
			    (mapcar
			     (lambda (id)
			       (cons (car elem) id))
			     (cdr (assq (cdr elem) tellstick-room-ids))))))
	  ;; On the form '(on apartment room-bedroom).
	  (pop action)
	  (dolist (room action)
	    (setq codes (append codes
				(mapcar
				 (lambda (id)
				   (cons command id))
				 (cdr (assq room tellstick-room-ids)))))))
	(dolist (host tellstick-transmitters)
	  (message "Sending command to %s" host)
	  (dolist (elem codes)
	    (eval-at
	     "lights" host 8700
	     `(tellstick-transmit
	       ,(tellstick-make-command
		 tellstick-room-code (cdr elem) (car elem)
		 (and (eq (car elem) 'on)
		      ;; If it's a dimmer, we have to send the signal
		      ;; strength.
		      (member (cdr elem) tellstick-dimmers)
		      15)))))))))))

(defun tellstick-switch-id (id action &optional dim)
  (when (eq action :on)
    (setq action 'on))
  (when (eq action :off)
    (setq action 'off))
  (when (numberp action)
    (setq action
	  (if (zerop action)
	      'off
	    'on)))
  (tellstick-send
   (tellstick-make-command
    tellstick-room-code id action
    (and (eq action 'on)
	 ;; If it's a dimmer, we have to send the signal
	 ;; strength.
	 (member id tellstick-dimmers)
	 15))))

(defun tellstick-switch-room (rooms action)
  "Perform ACTION (off/on) on ROOMS.
If TIMES is non-nil, it should be a number of times to do this."
  (when (eq action :off)
    (setq action 'off))
  (when (eq action :on)
    (setq action 'on))
  (let ((strings nil))
    (when (atom rooms)
      (setq rooms (list rooms)))
    (let (codes)
      (dolist (room rooms)
	(setq codes (append codes (cdr (assq room tellstick-room-ids)))))
      (dolist (host tellstick-transmitters)
	(message "%s Sending %s command to %s"
		 (format-time-string "%FT%T") action host)
	(dolist (id codes)
	  (eval-at
	   "lights" host 8700
	   `(tellstick-transmit
	     ,(tellstick-make-command
	       tellstick-room-code id action
	       (and (eq action 'on)
		    ;; If it's a dimmer, we have to send the signal
		    ;; strength.
		    (member id tellstick-dimmers)
		    15)))))))))

(defun tellstick-switch (action)
  (cond
   ((or (eq action 1)
	(eq action :on))
    (setq action 'on))
   ((or (eq action 0)
	(eq action :off))
    (setq action 'off)))
  (tellstick-switch-room '(tv living bedroom kitchen) action))

(defun tellstick-server (&optional model)
  (interactive)
  (when (eq model :duo)
    (setq tellstick-model 'duo)
    (tellstick-start-reading))
  (run-with-timer 0.1 0.1 'tellstick-queue-runner)
  (start-eval-server "lights" 8700
		     '(tellstick-transmit
		       tellstick-switch-room
		       tellstick-switch-id)))

(defun tellstick-central-server ()
  (interactive)
  (run-with-timer 0.1 0.1 'tellstick-central-queue)
  (setq eval-server-debug 1)
  (start-eval-server "lights" 8701
		     '(tellstick-switch-room
		       tellstick-receive-command
		       tellstick-execute-input)))

(provide 'tellstick)

;;; tellstick.el ends here
;(progn (load "tellstick") (tellstick-server :duo))
