(in-package :tagenvs)
(defparameter *year-articles-alist* nil)
(defparameter *year-month-articles-alist* nil)
(defparameter *tag-articles-alist* nil)
;; this does not work on windows boxes
;; TODO: make some (file-uri-to-path) function to address this
;; or whatever abstraction you can come up with at the moment
(defun blog-root-url (&key type)
  (let ((local-root-url
          (let* ((blog-root-relative-path
                   (make-pathname :directory '(:relative "blog")))
                 (blog-root-absolute-path
                   (merge-pathnames blog-root-relative-path))
                 (blog-root-absolute-path-string
                   (princ-to-string blog-root-absolute-path)))
            (format nil "file://~A" blog-root-absolute-path-string)))
        (remote-root-url
          "https://alozno-bazaar.github.io/xml-examples/blog-post/blog/"))
    (case type
      (remote remote-root-url)
      (local local-root-url))))

;; type 'local to run on your machine
;; type 'remote for deployment (here deployed on github pages)
(defparameter *blog-root-url* (blog-root-url :type 'local))
(defparameter month-names (make-array 12 :initial-contents '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
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
(defun add-to-tag-page (tag path)
  (let ((tag-entry (assoc tag *tag-articles-alist* :test #'string=)))
    (if tag-entry
        (push path (cdr tag-entry))
        (push (list tag path) *tag-articles-alist*))))
(defun link-to-tag (tag) (format nil "~Atags/~A.html" *blog-root-url* tag))
(defun link-to-year (year) (format nil "~Aposts/~A/all-year.html" *blog-root-url* year))

(defun link-to-month (&key year month)
  (format nil "~Aposts/~A/~A/all-month.html" *blog-root-url* year (month-name month)))
(defun link-to-blog (&key year month title)
  (format nil "~Aposts/~A/~A/~A.html" *blog-root-url* year (month-name month) title))
(defun month-name (number) (svref month-names (1- number)))

(with-html
    (defun render-blog-post (filepath &key title author date tags body)
      (render-to-file filepath
                      (html
                       (head
                        (meta :charset "utf8" :lang "en")
                        (title "some blog post"))
                       (body
                        (h1 :class "blog-title" title)
                        
                        (div :class "below-the-title" :style "text-align:right"
                             (h3 "author : " author)
                             (h3 "date : " (format nil "~A/~A/~A"
                                                   (date-year date)
                                                   (date-month date)
                                                   (date-day date))))
                        
                        (div :class "blog-main-body" body)
                        
                        (div :class "blog-tags"
                             (ul
                              (dolist (tag tags)
                                (li (a :href (link-to-tag tag) tag)))))))))
  ;; and finally
  (defun add-blog-post (&key (title "defalt title")
                          (date (make-date :year 1970 :month 1 :day 1))
                          (author "default author")
                          (tags nil)
                          (body nil))
    (let* ((blog-file-directory (blog-directory-path date))
           (blog-file-path (file-in-directory title blog-file-directory)))
      (ensure-directories-exist (make-blog-directory date title))
      (add-to-year-archive date blog-file-path)
      (add-to-year-month-archive date blog-file-path)
      (dolist (tag tags)
        (add-to-tag-page tag blog-file-path))
      (render-blog-post blog-file-path
                        :title title
                        :author author
                        :date date
                        :tags tags
                        :body body))))


;; blogs are already rendered, so render the archive and tag pages


(defun blog-directory-path (date)
  (when date nil))
(defun file-in-directory (title blog-file-directory)
  (when (or title blog-file-directory) nil))
(defun make-blog-directory (date title)
  (when (or date title) nil))
