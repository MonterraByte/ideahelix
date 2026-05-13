;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(ns fominok.ideahelix.editor.registers
  (:require
    [clojure.string :as str]
    [fominok.ideahelix.editor.selection :refer :all])
  (:import
    (java.awt
      Toolkit)
    (java.awt.datatransfer
      DataFlavor
      StringSelection)))


(defn- selected-strings
  [editor document]
  (let [model (.getCaretModel editor)]
    (let [strings* (transient [])]
      (.runForEachCaret
        model
        (fn [caret] (conj! strings* (.getText document (.getSelectionRange caret)))))
      (persistent! strings*))))


(defn- paste-strings
  [strings editor document & {:keys [before] :or {before false}}]
  (let [strings (concat strings (some-> (last strings) repeat))
        pairs (map (fn [caret string] [(ihx-selection document caret) string])
                   (.. editor getCaretModel getAllCarets)
                   strings)]
    (when (not (empty? strings))
      (doseq [[selection string] pairs
              :let [pos (if before
                          (max 0 (min (:anchor selection) (:offset selection)))
                          (min (.getTextLength document)
                               (inc (max (:anchor selection) (:offset selection)))))]]
        (.insertString document pos string)
        (-> selection
            (assoc :anchor pos)
            (assoc :offset (dec (+ pos (count string))))
            (ihx-apply-selection! document))))))


(defn copy-to-register
  [registers editor document & {:keys [register] :or {register \"}}]
  (let [strings (selected-strings editor document)]
    (assoc registers register strings)))


(defn paste-register
  [registers editor document & {:keys [register select] :or {register \" select false}}]
  (let [register-contents (get registers register)]
    (paste-strings register-contents editor document)))


(defn copy-to-clipboard
  [editor document]
  (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))
        contents (str/join "\n" (selected-strings editor document))]
    (.setContents clipboard (StringSelection. contents) nil)))


(defn paste-clipboard
  [editor document & {:keys [before] :or {before false}}]
  (let [clipboard (.getSystemClipboard (Toolkit/getDefaultToolkit))
        transferable (.getContents clipboard nil)]
    (when (and transferable (.isDataFlavorSupported transferable DataFlavor/stringFlavor))
      (paste-strings [(.getTransferData transferable DataFlavor/stringFlavor)] editor document :before before))))
