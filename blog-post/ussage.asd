#|
this system is one file
the whole point of this defsystem is making asdf load the xmltags package before loading the example usage file
|#
(defsystem :ussage ;; mispelling is intentional
  :author "somebody that you used to know"
  :license "GPLv2"
  :version "0"
  :depends-on ("xmltags")
  :components ((:file "example")))
