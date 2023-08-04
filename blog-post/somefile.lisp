(in-package :tagenvs)
(defparameter *blog-root* (make-pathname :directory '(:relative "blog")))
(ensure-directories-exist *blog-root*)
(defparameter *year-articles-alist* nil)
(defparameter *year-month-articles-alist* nil)
(defparameter *tag-posts-alist* nil)
(defstruct date year month day)
(defun add-to-year-archive (date path)
  (let ((year-entry (assoc (date-year date) *year-articles-alist*)))
    (if year-entry
        (push path (cdr year-entry))
        (push (list (date-year date) path) *year-articles-alist*))))

(defun add-to-year-month-archive (date path)
  (let ((year (date-year date))
        (month (date-month date))
        (year-entry (assoc (date-year date) *year-month-articles-alist*)))
    (unless year-entry
      (push (list year) *year-month-articles-alist*)
      (setf year-entry (car *year-month-articles-alist*)))
    (let ((month-entry
            (assoc month (cdr year-entry) :test #'string-equal)))
      (unless month-entry
        (push (list month) (cdr year-entry))
        (setf month-entry (cadr year-entry)))
      (push path (cdr month-entry)))))
(defun add-to-year-archive (tag path)
  (let ((tag-entry (assoc tag *year-articles-alist*)))
    (if tag-entry
        (push path (cdr year-entry))
        (push (list (date-year date) path) *year-articles-alist*))))
(with-html
  
  ;; and finally
  (defun add-blog-post (&key (title "defalt title")
                          (date (make-date 1970 1 1))
                          (author "default author")
                          (tags nil)
                          (body nil))
    (let* ((blog-file-directory-path (blog-directory-path date))
           (blog-file-path (file-in-directory title blog-file-directory)))
      (ensure-directories-exist (make-blog-directory date title))
      (add-to-year-archive date blog-file-path)
      (add-to-year-month-archive date blog-file-path)
      (dolist (tag tags)
        (add-to-tag-page tag blog-file-path))
      (render-blog-post blog-file-path
                        :author author
                        :date date
                        :tags tags
                        :body body)))


 ;; blogs are already rendered, so render the archive and tag pages
