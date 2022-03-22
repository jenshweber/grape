;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(use 'grape.core)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
_
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;nil&quot;</span>","value":"\"nil\""}
;; <=

;; @@
(viewgraph _)
;; @@
;; =>
;;; {"type":"html","content":"<img src=\"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAsAAAALCAYAAACprHcmAAAAF0lEQVR42mP4TzxYxTCqeHAqXkUkLgIAsQ/a+x4pdTQAAAAASUVORK5CYII=\" width=\"11\" height=\"11\" alt=\"\" />","value":"#object[java.awt.image.BufferedImage 0x205e665d \"BufferedImage@205e665d: type = 6 ColorModel: #pixelBits = 32 numComponents = 4 color space = java.awt.color.ICC_ColorSpace@4b313cd7 transparency = 3 has alpha = true isAlphaPre = false ByteInterleavedRaster: width = 11 height = 11 #numDataElements 4 dataOff[0] = 3\"]"}
;; <=

;; @@
(defn viewgraph [g]
  (-> (any _) viewquery))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/viewgraph</span>","value":"#'user/viewgraph"}
;; <=

;; @@
(viewgraph _)
;; @@
;; =>
;;; {"type":"html","content":"<img src=\"data:image/png;base64,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\" width=\"157\" height=\"133\" alt=\"\" />","value":"#object[java.awt.image.BufferedImage 0x18a43aa \"BufferedImage@18a43aa: type = 5 ColorModel: #pixelBits = 24 numComponents = 3 color space = java.awt.color.ICC_ColorSpace@4b313cd7 transparency = 1 has alpha = false isAlphaPre = false ByteInterleavedRaster: width = 157 height = 133 #numDataElements 3 dataOff[0] = 2\"]"}
;; <=

;; @@
(commit _ "save")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;8a80ecaf-6b94-42cc-87b8-63dda01519a8&quot;</span>","value":"\"8a80ecaf-6b94-42cc-87b8-63dda01519a8\""}
;; <=

;; @@
(-> (start) viewgraph)
;; @@

;; @@
  (rule 'hello
        :create (pattern
                 (node :label "Hello")))
;; @@
;; =>
;;; {"type":"html","content":"<img src=\"data:image/png;base64,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\" width=\"147\" height=\"133\" alt=\"\" />","value":"#object[java.awt.image.BufferedImage 0x2c15b6de \"BufferedImage@2c15b6de: type = 6 ColorModel: #pixelBits = 32 numComponents = 4 color space = java.awt.color.ICC_ColorSpace@4b313cd7 transparency = 3 has alpha = true isAlphaPre = false ByteInterleavedRaster: width = 147 height = 133 #numDataElements 4 dataOff[0] = 3\"]"}
;; <=

;; @@
(newgraph)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;7358f529-c74f-4695-a490-3409aad20a5d&quot;</span>","value":"\"7358f529-c74f-4695-a490-3409aad20a5d\""}
;; <=

;; @@
(hello _)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;8a80ecaf-6b94-42cc-87b8-63dda01519a8&quot;</span>","value":"\"8a80ecaf-6b94-42cc-87b8-63dda01519a8\""}
;; <=

;; @@
(commit _ "save")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-record'>#clojurewerkz.neocons.rest.records.CypherQueryResponse{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:data</span>","value":":data"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:data []]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:columns</span>","value":":columns"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:columns []]"}],"value":"#clojurewerkz.neocons.rest.records.CypherQueryResponse{:data [], :columns []}"}
;; <=
