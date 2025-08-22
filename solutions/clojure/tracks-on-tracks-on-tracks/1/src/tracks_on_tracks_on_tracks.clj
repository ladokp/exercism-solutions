(ns tracks-on-tracks-on-tracks)

(defn new-list
  "Creates an empty list of languages to practice."
  []
  (list)
  )

(defn add-language
  "Adds a language to the list."
  [lang-list lang]
  (conj lang-list lang)
  )

(defn first-language
  "Returns the first language on the list."
  [lang-list]
  (nth lang-list 0)
  )

(defn remove-language
  "Removes the first language added to the list."
  [lang-list]
  (pop lang-list)
  )

(defn count-languages
  "Returns the total number of languages on the list."
  [lang-list]
  (count lang-list)
  )

(defn learning-list
  "Creates an empty list, adds Clojure and Lisp, removes Lisp, adds
  Java and JavaScript, then finally returns a count of the total number
  of languages."
  []
  (def language-list (add-language (list) "Clojure"))  
  (def language-list (add-language language-list "Lisp"))
  (def language-list (remove-language language-list))
  (def language-list (add-language language-list "Java"))
  (def language-list (add-language language-list "JavaScript"))
  (count-languages language-list)
  )
