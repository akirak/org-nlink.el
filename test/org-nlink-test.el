;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-nlink)

(describe "org-nlink--sanitize-target"
  (it "replaces newlines"
    (expect (org-nlink--sanitize-target "Hello,\nJohn")
            :to-equal "Hello, John"))
  (it "downcases a single word"
    (expect (org-nlink--sanitize-target "Trees")
            :to-equal "trees"))
  (it "retains acronyms"
    (expect (org-nlink--sanitize-target "ABC")
            :to-equal "ABC")
    (expect (org-nlink--sanitize-target "S.H.I.E.L.D.")
            :to-equal "S.H.I.E.L.D."))
  (it "retains words that look like proper nouns"
    (expect (org-nlink--sanitize-target "Ho Chi Minh")
            :to-equal "Ho Chi Minh"))
  (it "downcases words"
    (expect (org-nlink--sanitize-target "Normal person")
            :to-equal "normal person")))

(provide 'org-nlink-test)
