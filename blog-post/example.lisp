(in-package :tagenvs)
(defparameter *year-articles-alist* nil)
(defparameter *year-month-articles-alist* nil)
(defparameter *tag-articles-alist* nil)
(defparameter *blog-root-absolute-path*
  (let ((blog-root-relative-path
          (make-pathname :directory '(:relative "blog"))))
    (merge-pathnames blog-root-relative-path)))

;; this does not work on windows boxes
;; TODO: make some (file-uri-to-path) function to address this
;; or whatever abstraction you can come up with at the moment
;; defstruct uri?
(defun blog-root-url (&key type)
  (let ((local-root-url
          (let ((blog-root-absolute-path-string
                  (princ-to-string *blog-root-absolute-path*)))
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
(defun mkdat (a b c) (make-date :year a :month b :day c))

;; optimal? no
;; useful? HELL YEAH
(defun cat2 (s1 s2) (format nil "~A~A" s1 s2))
(defun catn (&rest ss) (reduce #'cat2 ss :initial-value ""))

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
            (assoc month (cdr year-entry)))) ; :test #'string-equal)))
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
  (defun add-blog-post (&key (title "defalt.html")
                          (date (make-date :year 1970 :month 1 :day 1))
                          (author "default author")
                          (tags nil)
                          (body nil))
    (let* ((blog-file-directory (blog-directory-path date))
           (blog-file-path (file-in-directory title blog-file-directory)))
      (ensure-directories-exist blog-file-directory)
      (add-to-year-archive date blog-file-path)
      (add-to-year-month-archive date blog-file-path)
      (dolist (tag tags)
        (add-to-tag-page tag blog-file-path))
      (render-blog-post (princ-to-string blog-file-path)
                        :title title
                        :author author
                        :date date
                        :tags tags
                        :body body)))

  (defun test-run ()
    (add-blog-post :title "mamma.html"
                   :author "mia"
                   :date (mkdat 2001 9 11)
                   :tags '("very" "sad")
                   :body (render-string (h1 "ok")
                                        (h2 "not ok")
                                        (div :class "flex"
                                             (h4 "whoami")
                                             (p "root"))))
    (add-blog-post :title "kite.html"
                   :author "mmuort"
                   :date (mkdat 2000 10 10)
                   :tags '("tarantella" "ultras")
                   :body (render-string (h1 "forza napoli")
                                        (ul
                                         (dolist (x '("na bandiera tutta azzura"
                                                      "ca rassomiglia cielo"
                                                      "è un mare sta città"
                                                      "forza Napoli"))
                                           (li :class "box" x))
                                         (li :class "bot" (a :href "goatse.cx" "dat ass")))))

    (add-blog-post :tags '("ei" "fu" "tarantella")
                   :body "siccome immobile"))

  (defun render-archives ()
    (render-index)
    (render-tag-pages)
    (render-year-archives)
    (render-year-month-archives)
    )

  (defun render-index () nil)

  ;; some dry got wet with these
  (defun render-tag-pages ()
    (ensure-directories-exist (format nil "~Atags/" *blog-root-absolute-path*))
    (dolist (ta *tag-articles-alist*)
      (let ((tag (car ta))
            (posts (cdr ta)))
        (render-to-file (format nil "~Atags/~A.html" *blog-root-absolute-path* tag)
                        (base-archive :title (format nil "blogs tagged ~A" tag)
                                      :lst (mapcar #'princ-to-string posts))))))
        
  (defun render-year-archives ()
    (ensure-directories-exist (format nil "~Aposts/" *blog-root-absolute-path*))
    (dolist (ye *year-articles-alist*)
      (let ((year (car ye))
            (posts (cdr ye)))
        (ensure-directories-exist (format nil "~Aposts/~A/" *blog-root-absolute-path* year))
        (render-to-file (format nil "~Aposts/~A/all-year.html" *blog-root-absolute-path* year)
                        (base-archive :title (format nil "blogs in year ~A" year)
                                      :lst (mapcar #'princ-to-string posts))))))
  
  ;; this function looks BAD
  ;; REALLY BAD
  ;; probably syntomatic of a very lassiez lister attitude towards the usage of data structures within the code at hand
  
  (defun render-year-month-archives ()
    (ensure-directories-exist (cat2 *blog-root-absolute-path* "posts/"))
    (dolist (yemo *year-month-articles-alist*)
      (let ((year (car yemo))
            (months (cdr yemo)))
        (dolist (m months)
          (let ((month (car m))
                (posts (cdr m)))
            (let ((month-dir (catn *blog-root-absolute-path* "posts/"
                                   year "/" (month-name month) "/")))
              (ensure-directories-exist month-dir)
              (render-to-file (cat2 month-dir "all-month.html")
                              (base-archive :title (catn "posts in month " (month-name month)
                                                         " of year " year)
                                            :lst (mapcar #'princ-to-string posts)))))))))

  (defun base-archive (&key title lst)
    (html
     (head
      (meta :charset "utf8")
      (title title))
     (body
      (h1 :class "archive-head" title)
      (ul :class "archive-list"
          (dolist (l lst)
            (li l))))))
  )

;; blogs are already rendered, so render the archive and tag pages


(defun blog-directory-path (date)
  (merge-pathnames 
                   (make-pathname :directory `(:relative "posts"
                                                ,(princ-to-string (date-year date))
                                                ,(month-name (date-month date))))
                   *blog-root-absolute-path*))
                   
  
(defun file-in-directory (filename blog-file-directory)
  (multiple-value-bind (name type) (split-last-dot filename)
    (merge-pathnames blog-file-directory
                     (if type (make-pathname :name name :type type)
                         (make-pathname :name name)))))

(defun split-last-dot (str)
  (let ((lastdot (position #\. str :from-end t)))
    (if lastdot
        (values (subseq str 0 lastdot) (subseq str (1+ lastdot)))
        (values str nil))))

(defun make-blog-directory (date title)
  (when (or date title) nil))
