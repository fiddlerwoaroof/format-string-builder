(in-package #:format-string-builder)

(defclass format-string-command ()
  ((at-p :initarg :at-p :accessor at-p :initform nil)
   (colon-p :initarg :colon-p :accessor colon-p :initform nil)))

(defclass simple-format-string-command (format-string-command)
  ((format-char :initarg :format-char :accessor format-char)
   (modifiers :initarg :modifiers :accessor modifiers :initform nil)))

(defclass compound-format-string-command (format-string-command)
  ((start-char :initarg :start-char :accessor start-char)
   (contents :initarg :contents :accessor contents :initform nil)
   (end-char :initarg :end-char :accessor end-char)
   (modifiers :initarg :modifiers :accessor modifiers :initform nil)))

(defclass sectioned-format-string-command (compound-format-string-command)
  ())

(defmethod print-object ((obj compound-format-string-command) s)
  (declare (optimize (debug 3)))
  (print-unreadable-object (obj s :type t :identity t)
    (format s "~:@{\"~:[~;:~]~:[~;@~]~a...~a\" modifiers: ~s~}"
            (mapcar (lambda (x) (funcall x obj))
                    '(colon-p at-p start-char end-char modifiers)))))

(defgeneric print-format-modifiers (command stream)
  (:method ((command format-string-command) s)
     ;; TODO: format here causes inconsistent behavior :p
     (mapl (lambda (x)
             (princ (car x) s)
             (when (cdr x) (princ #\, s)))
           (modifiers command))))

(defgeneric print-format-representation (command stream)
  (:documentation "Prints the appropriate control sequence to the stream passed in
                   The :before method will print the Tilde."))

(defmethod print-format-representation ((commands list) s)
  (dolist (command commands)
    (print-format-representation command s)))

(defmethod print-format-representation ((literal character) s)
  (princ literal s))

(defmethod print-format-representation ((literal integer) s)
  (princ literal s))

(defmethod print-format-representation ((literal string) s)
  (princ literal s))

(defmethod print-format-representation :around ((command format-string-command) s)
  (princ #\~ s)
  (print-format-modifiers command s)
  (when (colon-p command)
    (princ #\: s))
  (when (at-p command)
    (princ #\@ s))
  (call-next-method))

(defmethod print-format-representation ((command simple-format-string-command) s)
  (princ (format-char command) s))

(defmethod print-format-representation :before ((command compound-format-string-command) s)
  (princ (start-char command) s))

(defmethod print-format-representation ((command compound-format-string-command) s)
  (print-format-representation (contents command) s))

(defmethod print-format-representation :after ((command compound-format-string-command) s)
  (princ #\~ s)
  (princ (end-char command) s))

(defmethod print-format-representation ((command sectioned-format-string-command) s)
  (mapcar (op (princ _ s))
	  (intersperse "~;" (contents command))))

;;; TODO: should this be a generic function?
(defun convert-modifier (modifier)
  (flet ((validate-modifier (modifier)
           (when (member (elt (string modifier) 0) '(#\: #\@ #\,))
             (error "Invalid modifier: ~s" modifier))
           modifier))
    (typecase modifier
      (character  (concatenate 'string "'" (string (validate-modifier modifier))))
      (string (validate-modifier modifier))
      (null "")
      (t modifier))))

(#-dev defvar
 #+dev defparameter *format-string-registry* (make-hash-table))

(defun generate-function-defs ()
  (loop for name being the hash-keys in *format-string-registry* using (hash-value spec)
        collect `(,name (&key colon-p at-p)
                        (make-instance ,@spec :colon-p colon-p :at-p at-p))))

(defgeneric dispatch-command (command args)
  (:method ((command simple-format-string-command) args)
   (setf (modifiers command) (mapcar #'convert-modifier args))
   command)
  (:method ((command compound-format-string-command) args)
   (destructuring-bind ((&rest modifiers) &rest contents) args
     (setf (modifiers command) (mapcar #'convert-modifier modifiers))
     (setf (contents command) (mapcar #'form-step contents))
     command)))

(defun form-step (form)
  (flet ((dispatch-keyword (keyword &optional args)
           (if-let ((spec (gethash keyword *format-string-registry*)))
             (let ((result (apply #'make-instance spec)))
               (when args
                 (dispatch-command result args))
               result))))
    (etypecase form
      (list (cond ((null form) nil)
                  ((keywordp (car form)) (dispatch-keyword (car form) (cdr form)))))
      ((or character string) form)
      (keyword (dispatch-keyword form)))))

(defun make-format-string (forms)
  (with-output-to-string (*format-stream*)
    (print-format-representation
      (mapcar #'form-step (macroexpand forms))
      *format-stream*)))

(defun define-simple-format-char (name format-char &key at-p colon-p)
  (setf (gethash (intern (string name) :keyword)
                 *format-string-registry*)
        `(simple-format-string-command :format-char ,format-char
                                       :at-p ,at-p
                                       :colon-p ,colon-p)))

(defun define-compound-format-char (name start-char end-char &key at-p colon-p)
  (setf (gethash (intern (string name) :keyword)
                 *format-string-registry*)
        `(compound-format-string-command :start-char ,start-char
                                         :end-char ,end-char
                                         :at-p ,at-p
                                         :colon-p ,colon-p)))

(defun define-sectioned-format-char (name start-char end-char &key at-p colon-p)
  (setf (gethash (intern (string name) :keyword)
		 *format-string-registry*)
	`(sectioned-format-string-command :start-char ,start-char
					  :end-char ,end-char
					  :at-p ,at-p
					  :colon-p ,colon-p)))

(defmacro define-compound-format-chars (&body specs)
  `(progn
     ,@(loop for spec in specs
	     collect `(define-compound-format-char ,@spec))))

(defmacro define-simple-format-chars (&body specs)
  `(progn
     ,@(loop for spec in specs
             collect `(define-simple-format-char ,@spec))))

(defmacro define-format-chars (&body body)
  `(macrolet ((:simple (name (char) &key at-p colon-p)
                `(define-simple-format-char ,name ,char
                                            :at-p ,at-p :colon-p ,colon-p))
              (:simples (&rest specs)
                (list* 'progn
                       (mapcar (lambda (spec)
                                 `(:simple ,@spec))
                               specs)))
              (:compound (name (start-char end-char) &key at-p colon-p)
                `(define-compound-format-char ,name ,start-char ,end-char
                                              :at-p ,at-p :colon-p ,colon-p))
              (:compounds (&rest specs)
                (list* 'progn
                       (mapcar (lambda (spec)
                                 `(:compound ,@spec))
                               specs)))
	      (:sectioned (name (start-char end-char) &key at-p colon-p)
		`(define-sectioned-format-char ,name ,start-char ,end-char
					       :at-p ,at-p :colon-p ,colon-p)))
     ,@body))

(defmacro format* (stream format-spec &rest args)
  `(format ,stream
           ,(make-format-string format-spec)
           ,@args))

(defmacro define-message (name (stream-arg &rest args) &body spec)
  "Define a function called NAME that takes a stream argument and a
variable argument list that formats the arguments according to the
spec passed as the body."
  (flet ((get-argument-names (arg-list)
           (loop for s in arg-list
                 when (and (symbolp s) (not (char= (elt (symbol-name s) 0) #\&))) collect s
                 when (consp s) collect (car s))))
    (with-gensyms (fs the-rest)
      `(let ((,fs ,(make-format-string spec)))
         (defun ,name (,stream-arg ,@args &rest ,the-rest)
           (apply #'format ,stream-arg ,fs ,@(get-argument-names args) ,the-rest))))))

(define-format-chars

  ;; Iteration characters.
  (:compounds
    (:map (#\{ #\}))
    (:rest (#\{ #\}) :at-p t)

    (:ap (#\{ #\}) :colon-p t)
    (:apply (#\{ #\}) :colon-p t)

    (:aprest (#\{ #\}) :at-p t :colon-p t)
    (:apply-rest (#\{ #\}) :at-p t :colon-p t))

  ;; Alignment characters.
  (:compounds
    (:spread (#\< #\>))

    (:ljust (#\< #\>) :at-p t)
    (:left (#\< #\>) :at-p t)

    (:rjust (#\< #\>) :colon-p t)
    (:right (#\< #\>) :colon-p t)

    (:cjust (#\< #\>) :at-p t :colon-p t)
    (:center (#\< #\>) :at-p t :colon-p t))

  ;; Conditional output
  (:sectioned :y-or-n (#\[ #\]) :colon-p t)

  ;; Case printing characters.
  (:compounds
    (:lowercase (#\( #\)))
    (:downcase (#\( #\)))
    (:uppercase (#\( #\)) :at-p t :colon-p t)
    (:upcase (#\( #\)) :at-p t :colon-p t)
    (:titlecase (#\( #\)) :colon-p t)
    (:capitalize (#\( #\)) :colon-p t)
    (:initialcap (#\( #\)) :at-p t))

  (:compounds
    (:own-line (#\& #\%)))

  (:simples
    (:str (#\a))
    (:repr (#\s))
    (:float (#\f))
    (:dec (#\d))
    (:decimal (#\d))
    (:hex (#\x))
    (:hexadecimal (#\d))
    (:oct (#\o))
    (:octal (#\o))
    (:currency (#\$))
    (:exit (#\^))
    (:go-to (#\*))
    (:end-section (#\;))
    (:fresh-line (#\&))
    (:ensure-line (#\&))
    (:new-line (#\%))))

(define-message hello (stream name)
  (:titlecase () "hello" #\space :str))

(define-message print-comma-separated (stream)
  (:own-line ()
   (:rest () :str :exit ", ")))
