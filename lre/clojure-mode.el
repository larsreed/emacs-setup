;;; clojure-mode.el --- Major mode for Clojure code

;; Copyright (C) 2007-2011 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;; Lennart Staflin <lenst@lysator.liu.se>
;; Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/clojure-mode
;; Version: 1.11.2
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, and navigation for the Clojure
;; language. (http://clojure.org)

;; Users of older Emacs (pre-22) should get version 1.4:
;; http://github.com/technomancy/clojure-mode/tree/1.4

;;; Installation:

;; Use package.el. You'll need to add Marmalade to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;; '("marmalade" . "http://marmalade-repo.org/packages/"))

;; If you use a version of Emacs prior to 24 that doesn't include
;; package.el, you can get it from http://bit.ly/pkg-el23. If you have
;; an older package.el installed from tromey.com, you should upgrade
;; in order to support installation from multiple sources.

;; Of course, it's possible to just place it on your load-path and
;; require it as well if you don't mind missing out on
;; byte-compilation and autoloads.

;; Using clojure-mode with paredit is highly recommended. It is also
;; available using package.el from the above archive.

;; Use paredit as you normally would with any other mode; for instance:
;;
;; ;; require or autoload paredit-mode
;; (defun turn-on-paredit () (paredit-mode 1))
;; (add-hook 'clojure-mode-hook 'turn-on-paredit)

;; See Swank Clojure (http://github.com/technomancy/swank-clojure) for
;; better interaction with subprocesses via SLIME.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl)

(defgroup clojure-mode nil
  "A mode for Clojure"
  :prefix "clojure-mode-"
  :group 'applications)

(defcustom clojure-mode-font-lock-comment-sexp nil
  "Set to non-nil in order to enable font-lock of (comment...)
forms. This option is experimental. Changing this will require a
restart (ie. M-x clojure-mode) of existing clojure mode buffers."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-mode-load-command "(clojure.core/load-file \"%s\")\n"
  "*Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior
Clojure to load that file."
  :type 'string
  :group 'clojure-mode)

(defcustom clojure-mode-use-backtracking-indent t
  "Set to non-nil to enable backtracking/context sensitive indentation."
  :type 'boolean
  :group 'clojure-mode)

(defcustom clojure-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :group 'clojure-mode)

(defvar clojure-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e\C-x" 'lisp-eval-defun)
    (define-key map "\C-x\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-e" 'lisp-eval-last-sexp)
    (define-key map "\C-c\C-l" 'clojure-load-file)
    (define-key map "\C-c\C-r" 'lisp-eval-region)
    (define-key map "\C-c\C-z" 'run-lisp)
    (define-key map (kbd "RET") 'reindent-then-newline-and-indent)
    (define-key map (kbd "C-c t") 'clojure-jump-to-test)
    map)
  "Keymap for Clojure mode. Inherits from `lisp-mode-shared-map'.")

(defvar clojure-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?~ "' " table)
    ;; can't safely make commas whitespace since it will apply even
    ;; inside string literals--ick!
    ;; (modify-syntax-entry ?, " " table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?^ "'" table)
    table))

(defvar clojure-mode-abbrev-table nil
  "Abbrev table used in clojure-mode buffers.")

(define-abbrev-table 'clojure-mode-abbrev-table ())

(defvar clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `clojure-load-file' or `clojure-compile-file' command.")

(defvar clojure-test-ns-segment-position -1
  "Which segment of the ns is \"test\" inserted in your test name convention.

Customize this depending on your project's conventions. Negative
numbers count from the end:

leiningen.compile -> leiningen.test.compile (uses 1)
clojure.http.client -> clojure.http.test.client (uses -1)")

(defun clojure-mode-version ()
  "Currently package.el doesn't support prerelease version numbers."
  "1.11.2")

;;;###autoload
(defun clojure-mode ()
  "Major mode for editing Clojure code - similar to Lisp mode.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs. Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map clojure-mode-map)
  (setq mode-name "Clojure"
        major-mode 'clojure-mode
        imenu-create-index-function
        (lambda ()
          (imenu--generic-function '((nil clojure-match-next-def 0))))
        local-abbrev-table clojure-mode-abbrev-table
        indent-tabs-mode nil)
  (lisp-mode-variables nil)
  (set-syntax-table clojure-mode-syntax-table)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (when (< emacs-major-version 24)
    (set (make-local-variable 'forward-sexp-function)
         'clojure-forward-sexp))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (set (make-local-variable 'inferior-lisp-program) "lein repl")

  (clojure-mode-font-lock-setup)

  (run-mode-hooks 'clojure-mode-hook)
  (run-hooks 'prog-mode-hook)

  ;; Enable curly braces when paredit is enabled in clojure-mode-hook
  (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
    (define-key clojure-mode-map "{" 'paredit-open-curly)
    (define-key clojure-mode-map "}" 'paredit-close-curly)))

(defun clojure-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load Clojure file: "
                                  clojure-prev-l/c-dir/file
                                  '(clojure-mode) t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq clojure-prev-l/c-dir/file (cons (file-name-directory file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inferior-lisp-proc)
                      (format clojure-mode-load-command file-name))
  (switch-to-lisp t))



(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next top-level definition.
Called by `imenu--generic-function'."
  (when (re-search-backward "^\\s *(def\\S *[ \n\t]+" nil t)
    (save-mark-and-excursion
      (goto-char (match-end 0))
      (when (looking-at "#?\\^")
        (let (forward-sexp-function) ; using the built-in one
          (forward-sexp))) ; skip the metadata
      (re-search-forward "[^ \n\t)]+"))))

(defun clojure-mode-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (interactive)
  (set (make-local-variable 'font-lock-multiline) t)
  (add-to-list 'font-lock-extend-region-functions
               'clojure-font-lock-extend-region-def t)

  (when clojure-mode-font-lock-comment-sexp
    (add-to-list 'font-lock-extend-region-functions
                 'clojure-font-lock-extend-region-comment t)
    (make-local-variable 'clojure-font-lock-keywords)
    (add-to-list 'clojure-font-lock-keywords
                 'clojure-font-lock-mark-comment t)
    (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil))

  (setq font-lock-defaults
        '(clojure-font-lock-keywords ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function))))

(defun clojure-font-lock-def-at-point (point)
  "Find the position range between the top-most def* and the
fourth element afterwards. Note that this means there's no
gaurantee of proper font locking in def* forms that are not at
top-level."
  (goto-char point)
  (condition-case nil
      (beginning-of-defun)
    (error nil))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (condition-case nil
          (progn
            ;; move forward as much as possible until failure (or success)
            (forward-char)
            (dotimes (i 4)
              (forward-sexp)))
        (error nil))
      (cons beg-def (point)))))

(defun clojure-font-lock-extend-region-def ()
  "Move fontification boundaries to always include the first four
elements of a def* forms."
  (let ((changed nil))
    (let ((def (clojure-font-lock-def-at-point font-lock-beg)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))

    (let ((def (clojure-font-lock-def-at-point font-lock-end)))
      (when def
        (destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun clojure-font-lock-extend-region-comment ()
  "Move fontification boundaries to always contain
entire (comment ..) sexp. Does not work if you have a
white-space between ( and comment, but that is omitted to make
this run faster."
  (let ((changed nil))
    (goto-char font-lock-beg)
    (condition-case nil (beginning-of-defun) (error nil))
    (let ((pos (re-search-forward "(comment\\>" font-lock-end t)))
      (when pos
        (forward-char -8)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point)
                changed t))
        (condition-case nil (forward-sexp) (error nil))
        (when (> (point) font-lock-end)
          (setq font-lock-end (point)
                changed t))))
    changed))

(defun clojure-font-lock-mark-comment (limit)
  "Marks all (comment ..) forms with font-lock-comment-face."
  (let (pos)
    (while (and (< (point) limit)
                (setq pos (re-search-forward "(comment\\>" limit t)))
      (when pos
        (forward-char -8)
        (condition-case nil
            (add-text-properties (1+ (point)) (progn
                                                (forward-sexp) (1- (point)))
                                 '(face font-lock-comment-face multiline t))
          (error (forward-char 8))))))
  nil)

(defconst clojure-font-lock-keywords
  (eval-when-compile
    `( ;; Definitions.
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defn" "defn-" "def" "def-" "defonce"
                              "defmulti" "defmethod" "defmacro"
                              "defstruct" "deftype" "defprotocol"
                              "defrecord" "deftest"
                              "slice" "def\\[a-z\\]"
                              "defalias" "defhinted" "defmacro-"
                              "defn-memo" "defnk" "defonce-"
                              "defstruct-" "defunbound" "defunbound-"
                              "defvar" "defvar-"))
                ;; Function declarations.
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; Deprecated functions
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("add-watcher" "remove-watcher" "add-classpath") t)
         "\\>")
       1 font-lock-warning-face)
      ;; Control structures
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("let" "letfn" "do"
            "case" "cond" "condp"
            "for" "loop" "recur"
            "when" "when-not" "when-let" "when-first"
            "if" "if-let" "if-not"
            "." ".." "->" "->>" "doto"
            "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "load" "import" "unimport" "ns" "in-ns" "refer"
            "try" "catch" "finally" "throw"
            "with-open" "with-local-vars" "binding"
            "gen-class" "gen-and-load-class" "gen-and-save-class"
            "handler-case" "handle") t)
         "\\>")
       1 font-lock-builtin-face)
      ;; Built-ins
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("*" "*1" "*2" "*3" "*agent*"
        "*allow-unresolved-vars*" "*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*"
        "*compile-path*" "*e" "*err*" "*file*" "*flush-on-newline*"
        "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
        "*print-dup*" "*print-length*" "*print-level*" "*print-meta*" "*print-readably*"
        "*read-eval*" "*source-path*" "*use-context-classloader*" "*warn-on-reflection*" "+"
        "-" "/"
        "<" "<=" "=" "==" ">"
        ">=" "accessor" "aclone"
        "agent" "agent-errors" "aget" "alength" "alias"
        "all-ns" "alter" "alter-meta!" "alter-var-root" "amap"
        "ancestors" "and" "apply" "areduce" "array-map"
        "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double"
        "aset-float" "aset-int" "aset-long" "aset-short" "assert"
        "assoc" "assoc!" "assoc-in" "associative?" "atom"
        "await" "await-for" "await1" "bases" "bean"
        "bigdec" "bigint" "binding" "bit-and" "bit-and-not"
        "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set"
        "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean"
        "boolean-array" "booleans" "bound-fn" "bound-fn*" "butlast"
        "byte" "byte-array" "bytes" "case" "cast" "char"
        "char-array" "char-escape-string" "char-name-string" "char?" "chars"
        "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first"
        "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?"
        "clear-agent-errors" "clojure-version" "coll?" "comment" "commute"
        "comp" "comparator" "compare" "compare-and-set!" "compile"
        "complement" "concat" "cond" "condp" "conj"
        "conj!" "cons" "constantly" "construct-proxy" "contains?"
        "count" "counted?" "create-ns" "create-struct" "cycle"
        "dec" "decimal?" "declare" "definline" "defmacro"
        "defmethod" "defmulti" "defn" "defn-" "defonce"
        "defstruct" "delay" "delay?" "deliver" "deref"
        "derive" "descendants" "destructure" "disj" "disj!"
        "dissoc" "dissoc!" "distinct" "distinct?" "doall"
        "doc" "dorun" "doseq" "dosync" "dotimes"
        "doto" "double" "double-array" "doubles" "drop"
        "drop-last" "drop-while" "empty" "empty?" "ensure"
        "enumeration-seq" "eval" "even?" "every?"
        "extend" "extend-protocol" "extend-type" "extends?" "extenders"
        "false?" "ffirst" "file-seq" "filter" "find" "find-doc"
        "find-ns" "find-var" "first" "flatten" "float" "float-array"
        "float?" "floats" "flush" "fn" "fn?"
        "fnext" "for" "force" "format" "future"
        "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?"
        "gen-class" "gen-interface" "gensym" "get" "get-in"
        "get-method" "get-proxy-class" "get-thread-bindings" "get-validator" "hash"
        "hash-map" "hash-set" "identical?" "identity" "if-let"
        "if-not" "ifn?" "import" "in-ns" "inc"
        "init-proxy" "instance?" "int" "int-array" "integer?"
        "interleave" "intern" "interpose" "into" "into-array"
        "ints" "io!" "isa?" "iterate" "iterator-seq"
        "juxt" "key" "keys" "keyword" "keyword?"
        "last" "lazy-cat" "lazy-seq" "let" "letfn"
        "line-seq" "list" "list*" "list?" "load"
        "load-file" "load-reader" "load-string" "loaded-libs" "locking"
        "long" "long-array" "longs" "loop" "macroexpand"
        "macroexpand-1" "make-array" "make-hierarchy" "map" "map?"
        "mapcat" "max" "max-key" "memfn" "memoize"
        "merge" "merge-with" "meta" "method-sig" "methods"
        "min" "min-key" "mod" "name" "namespace"
        "neg?" "newline" "next" "nfirst" "nil?"
        "nnext" "not" "not-any?" "not-empty" "not-every?"
        "not=" "ns" "ns-aliases" "ns-imports" "ns-interns"
        "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve"
        "ns-unalias" "ns-unmap" "nth" "nthnext" "num"
        "number?" "odd?" "or" "parents" "partial"
        "partition" "pcalls" "peek" "persistent!" "pmap"
        "pop" "pop!" "pop-thread-bindings" "pos?" "pr"
        "pr-str" "prefer-method" "prefers" "primitives-classnames" "print"
        "print-ctor" "print-doc" "print-dup" "print-method" "print-namespace-doc"
        "print-simple" "print-special-doc" "print-str" "printf" "println"
        "println-str" "prn" "prn-str" "promise" "proxy"
        "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super" "push-thread-bindings"
        "pvalues" "quot" "rand" "rand-int" "range"
        "ratio?" "rational?" "rationalize" "re-find" "re-groups"
        "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
        "read-line" "read-string" "reify" "reduce" "ref" "ref-history-count"
        "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure"
        "release-pending-sends" "rem" "remove" "remove-method" "remove-ns"
        "repeat" "repeatedly" "replace" "replicate"
        "require" "reset!" "reset-meta!" "resolve" "rest"
        "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq"
        "satisfies?" "second" "select-keys" "send" "send-off" "seq"
        "seq?" "seque" "sequence" "sequential?" "set"
        "set-validator!" "set?" "short" "short-array" "shorts"
        "shutdown-agents" "slurp" "some" "sort" "sort-by"
        "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?"
        "special-form-anchor" "special-symbol?" "split-at" "split-with" "str"
        "stream?" "string?" "struct" "struct-map" "subs"
        "subseq" "subvec" "supers" "swap!" "symbol"
        "symbol?" "sync" "syntax-symbol-anchor" "take" "take-last"
        "take-nth" "take-while" "test" "the-ns" "time"
        "to-array" "to-array-2d" "trampoline" "transient" "tree-seq"
        "true?" "type" "unchecked-add" "unchecked-dec" "unchecked-divide"
        "unchecked-inc" "unchecked-multiply" "unchecked-negate" "unchecked-remainder" "unchecked-subtract"
        "underive" "unquote" "unquote-splicing" "update-in" "update-proxy"
        "use" "val" "vals" "var-get" "var-set"
        "var?" "vary-meta" "vec" "vector" "vector?"
        "when" "when-first" "when-let" "when-not" "while"
        "with-bindings" "with-bindings*" "with-in-str" "with-loading-context" "with-local-vars"
        "with-meta" "with-open" "with-out-str" "with-precision" "xml-seq" "zipmap"
        ) t)
         "\\>")
       1 font-lock-variable-name-face)
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;;Other namespaces in clojure.jar
      (,(concat
         "(\\(?:\.*/\\)?"
         (regexp-opt
          '(;; clojure.inspector
        "atom?" "collection-tag" "get-child" "get-child-count" "inspect"
        "inspect-table" "inspect-tree" "is-leaf" "list-model" "list-provider"
        ;; clojure.main
        "load-script" "main" "repl" "repl-caught" "repl-exception"
        "repl-prompt" "repl-read" "skip-if-eol" "skip-whitespace" "with-bindings"
        ;; clojure.set
        "difference" "index" "intersection" "join" "map-invert"
        "project" "rename" "rename-keys" "select" "union"
        ;; clojure.stacktrace
        "e" "print-cause-trace" "print-stack-trace" "print-throwable" "print-trace-element"
        ;; clojure.template
        "do-template" "apply-template"
        ;; clojure.test
        "*initial-report-counters*" "*load-tests*" "*report-counters*" "*stack-trace-depth*" "*test-out*"
        "*testing-contexts*" "*testing-vars*" "are" "assert-any" "assert-expr"
        "assert-predicate" "compose-fixtures" "deftest" "deftest-" "file-position"
        "function?" "get-possibly-unbound-var" "inc-report-counter" "is" "join-fixtures"
        "report" "run-all-tests" "run-tests" "set-test" "successful?"
        "test-all-vars" "test-ns" "test-var" "testing" "testing-contexts-str"
        "testing-vars-str" "try-expr" "use-fixtures" "with-test" "with-test-out"
        ;; clojure.walk
        "keywordize-keys" "macroexpand-all" "postwalk" "postwalk-demo" "postwalk-replace"
        "prewalk" "prewalk-demo" "prewalk-replace" "stringify-keys" "walk"
        ;; clojure.xml
        "*current*" "*sb*" "*stack*" "*state*" "attrs"
        "content" "content-handler" "element" "emit" "emit-element"
        ;; clojure.zip
        "append-child" "branch?" "children" "down" "edit"
        "end?" "insert-child" "insert-left" "insert-right" "left"
        "leftmost" "lefts" "make-node" "next" "node"
        "path" "prev" "remove" "replace" "right"
        "rightmost" "rights" "root" "seq-zip" "up"
        ) t)
         "\\>")
       1 font-lock-type-face)
      ;; Constant values (keywords), including as metadata e.g. ^:static
      ("\\<^?:\\(\\sw\\|#\\)+\\>" 0 font-lock-constant-face)
      ;; Meta type annotation #^Type or ^Type
      ("#?^\\sw+" 0 font-lock-type-face)
      ("\\<io\\!\\>" 0 font-lock-warning-face)

      ;;Java interop highlighting
      ("\\<\\.[a-z][a-zA-Z0-9]*\\>" 0 font-lock-preprocessor-face) ;; .foo .barBaz .qux01
      ("\\<[A-Z][a-zA-Z0-9]*/[a-zA-Z0-9/$_]+\\>" 0 font-lock-preprocessor-face) ;; Foo Bar$Baz Qux_
      ("\\<[a-zA-Z]+\\.[a-zA-Z0-9._]*[A-Z]+[a-zA-Z0-9/.$]*\\>" 0 font-lock-preprocessor-face) ;; Foo/Bar foo.bar.Baz foo.Bar/baz
      ("[a-z]*[A-Z]+[a-z][a-zA-Z0-9$]*\\>" 0 font-lock-preprocessor-face) ;; fooBar
      ("\\<[A-Z][a-zA-Z0-9$]*\\.\\>" 0 font-lock-preprocessor-face))) ;; Foo. BarBaz. Qux$Quux. Corge9.


  "Default expressions to highlight in Clojure mode.")

;; Docstring positions
(put 'defn 'clojure-doc-string-elt 2)
(put 'defn- 'clojure-doc-string-elt 2)
(put 'defmulti 'clojure-doc-string-elt 2)
(put 'defmacro 'clojure-doc-string-elt 2)
(put 'definline 'clojure-doc-string-elt 2)
(put 'defprotocol 'clojure-doc-string-elt 2)

;; Docstring positions - contrib
(put 'defalias 'clojure-doc-string-elt 3)
(put 'defmacro- 'clojure-doc-string-elt 2)
(put 'defn-memo 'clojure-doc-string-elt 2)
(put 'defnk 'clojure-doc-string-elt 2)
(put 'defonce- 'clojure-doc-string-elt 3)
(put 'defunbound 'clojure-doc-string-elt 2)
(put 'defunbound- 'clojure-doc-string-elt 2)
(put 'defvar 'clojure-doc-string-elt 3)
(put 'defvar- 'clojure-doc-string-elt 3)



(defun clojure-forward-sexp (n)
  "Treat record literals like #user.Foo[1] and #user.Foo{:size 1}
as a single sexp so that slime will send them properly. Arguably
this behavior is unintuitive for the user pressing (eg) C-M-f
himself, but since these are single objects I think it's right."
  (let ((dir (if (> n 0) 1 -1))
        (forward-sexp-function nil)) ; force the built-in version
    (while (not (zerop n))
      (forward-sexp dir)
      (when (save-mark-and-excursion ; move back to see if we're in a record literal
              (and
               (condition-case nil
                   (progn (backward-sexp) 't)
                 ('scan-error nil))
               (looking-at "#\\w")))
        (forward-sexp dir)) ; if so, jump over it
      (setq n (- n dir)))))

(defun clojure-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
It is used when indenting a line within a function call, to see if the
called function says anything special about how to indent the line.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function
which has a non-nil property `lisp-indent-function',
that specifies how to do the indentation. The property value can be
* `defun', meaning indent `defun'-style;
* an integer N, meaning indent the first N arguments specially
like ordinary function arguments and then indent any further
arguments like a body;
* a function to call just as this function was called.
If that function returns nil, that means it doesn't specify
the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-mark-and-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp. Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (if (and (eq (char-after (point)) ?\[)
                   (eq (char-after (elt state 1)) ?\())
              (+ (current-column) 2) ;; this is probably inside a defn
            (current-column)))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            (open-paren (elt state 1))
            method)
        (setq method (get (intern-soft function) 'clojure-indent-function))

        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`\\(?:\\S +/\\)?def\\|with-"
                                      function)))
               (lisp-indent-defform state indent-point))

              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              (clojure-mode-use-backtracking-indent
               (clojure-backtracking-indent
                indent-point state normal-indent)))))))

(defun clojure-backtracking-indent (indent-point state normal-indent)
  "Experimental backtracking support. Will upwards in an sexp to
check for contextual indenting."
  (let (indent (path) (depth 0))
    (goto-char (elt state 1))
    (while (and (not indent)
                (< depth clojure-max-backtracking))
      (let ((containing-sexp (point)))
        (parse-partial-sexp (1+ containing-sexp) indent-point 1 t)
        (when (looking-at "\\sw\\|\\s_")
          (let* ((start (point))
                 (fn (buffer-substring start (progn (forward-sexp 1) (point))))
                 (meth (get (intern-soft fn) 'clojure-backtracking-indent)))
            (let ((n 0))
              (when (< (point) indent-point)
                (condition-case ()
                    (progn
                      (forward-sexp 1)
                      (while (< (point) indent-point)
                        (parse-partial-sexp (point) indent-point 1 t)
                        (incf n)
                        (forward-sexp 1)))
                  (error nil)))
              (push n path))
            (when meth
              (let ((def meth))
                (dolist (p path)
                  (if (and (listp def)
                           (< p (length def)))
                      (setq def (nth p def))
                    (if (listp def)
                        (setq def (car (last def)))
                      (setq def nil))))
                (goto-char (elt state 1))
                (when def
                  (setq indent (+ (current-column) def)))))))
        (goto-char containing-sexp)
        (condition-case ()
            (progn
              (backward-up-list 1)
              (incf depth))
          (error (setq depth clojure-max-backtracking)))))
    indent))

;; clojure backtracking indent is experimental and the format for these
;; entries are subject to change
(put 'implement 'clojure-backtracking-indent '(4 (2)))
(put 'letfn 'clojure-backtracking-indent '((2) 2))
(put 'proxy 'clojure-backtracking-indent '(4 4 (2)))
(put 'reify 'clojure-backtracking-indent '((2)))
(put 'deftype 'clojure-backtracking-indent '(4 4 (2)))
(put 'defrecord 'clojure-backtracking-indent '(4 4 (2)))
(put 'defprotocol 'clojure-backtracking-indent '(4 (2)))
(put 'extend-type 'clojure-backtracking-indent '(4 (2)))
(put 'extend-protocol 'clojure-backtracking-indent '(4 (2)))

(defun put-clojure-indent (sym indent)
  (put sym 'clojure-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                        (quote ,(first x)) ,(second x))) kvs)))

(defun add-custom-clojure-indents (name value)
  (setq clojure-defun-indents value)
  (mapcar (lambda (x)
            (put-clojure-indent x 'defun))
          value))

(defcustom clojure-defun-indents nil
  "List of symbols to give defun-style indentation to in Clojure
code, in addition to those that are built-in. You can use this to
get emacs to indent your own macros the same as it does the
built-ins like with-open. To set manually from lisp code,
use (put-clojure-indent 'some-symbol 'defun)."
  :type '(repeat symbol)
  :group 'clojure-mode
  :set 'add-custom-clojure-indents)

(define-clojure-indent
  ;; built-ins
  (ns 1)
  (fn 'defun)
  (def 'defun)
  (defn 'defun)
  (bound-fn 'defun)
  (if 1)
  (if-not 1)
  (case 1)
  (condp 2)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy 2)
  (with-open 1)
  (with-precision 1)
  (with-local-vars 1)

  (reify 'defun)
  (deftype 2)
  (defrecord 2)
  (defprotocol 1)
  (extend 1)
  (extend-protocol 1)
  (extend-type 1)

  (try 0)
  (catch 2)

  ;; binding forms
  (let 1)
  (letfn 1)
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)

  ;; data structures
  (defstruct 1)
  (struct-map 1)
  (assoc 1)

  (defmethod 'defun)

  ;; clojure.test
  (testing 1)
  (deftest 'defun)

  ;; contrib
  (handler-case 1)
  (handle 1)
  (dotrace 1)
  (deftrace 'defun))



(defconst clojure-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      ;; why is this here? oh (in-ns 'foo) or (ns+ :user)
      (zero-or-one (any ":'"))
      (group (one-or-more (not (any "()\"" whitespace))) word-end)))

;; for testing clojure-namespace-name-regex, you can evaluate this code and make
;; sure foo (or whatever the namespace name is) shows up in results. some of
;; these currently fail.
;; (mapcar (lambda (s) (let ((n (string-match clojure-namespace-name-regex s)))
;; (if n (match-string 4 s))))
;; '("(ns foo)"
;; "(ns
;; foo)"
;; "(ns foo.baz)"
;; "(ns ^:bar foo)"
;; "(ns ^:bar ^:baz foo)"
;; "(ns ^{:bar true} foo)"
;; "(ns #^{:bar true} foo)"
;; "(ns #^{:fail {}} foo)"
;; "(ns ^{:fail2 {}} foo.baz)"
;; "(ns ^{} foo)"
;; "(ns ^{:skip-wiki true}
;; aleph.netty
;; "
;; "(ns
;; foo)"
;; "foo"))



(defun clojure-insert-ns-form ()
  (interactive)
  (goto-char (point-min))
  (let* ((rel (car (last (split-string buffer-file-name "src/\\|test/"))))
         (relative (car (split-string rel "\\.clj")))
         (segments (split-string relative "/")))
    (insert (format "(ns %s)" (mapconcat #'identity segments ".")))))


;;; Slime help

(defvar clojure-project-root-file "project.clj")

;; Pipe to sh to work around mackosecks GUI Emacs $PATH issues.
(defvar clojure-swank-command (if (or (locate-file "lein" exec-path)
                                      (locate-file "lein.bat" exec-path))
                                  "lein jack-in %s"
                                "echo \"lein jack-in %s\" | sh"))

(defun clojure-jack-in-sentinel (process event)
  (let ((debug-on-error t))
    (error "Could not start swank server: %s"
           (with-current-buffer (process-buffer process)
             (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun clojure-jack-in ()
  (interactive)
  (setq slime-net-coding-system 'utf-8-unix)
  (lexical-let ((port (- 65535 (mod (caddr (current-time)) 4096)))
                (dir default-directory))
    (when (and (functionp 'slime-disconnect) (slime-current-connection))
      (slime-disconnect))
    (when (get-buffer "*swank*")
      (kill-buffer "*swank*"))
    (let* ((swank-cmd (format clojure-swank-command port))
           (proc (start-process-shell-command "swank" "*swank*" swank-cmd)))
      (set-process-sentinel (get-buffer-process "*swank*")
                            'clojure-jack-in-sentinel)
      (set-process-filter (get-buffer-process "*swank*")
                          (lambda (process output)
                            (with-current-buffer (process-buffer process)
                              (insert output))
                            (when (string-match "proceed to jack in" output)
                              (eval-buffer (process-buffer process))
                              (slime-connect "localhost" port)
                              (with-current-buffer (slime-output-buffer t)
                                (setq default-directory dir))
                              (set-process-sentinel process nil)
                              (set-process-filter process nil))))))
  (message "Starting swank server..."))

(defun clojure-find-ns ()
  (let ((regexp clojure-namespace-name-regex))
    (save-mark-and-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 4)))))

(defalias 'clojure-find-package 'clojure-find-ns)

(defun clojure-enable-slime ()
  (slime-mode t)
  (set (make-local-variable 'slime-find-buffer-package-function)
       'clojure-find-ns))

;;;###autoload
(defun clojure-enable-slime-on-existing-buffers ()
  (interactive)
  (add-hook 'clojure-mode-hook 'clojure-enable-slime)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'clojure-mode)
          (clojure-enable-slime))))))

;; Test navigation:

(defun clojure-underscores-for-hyphens (namespace)
  (replace-regexp-in-string "-" "_" namespace))

(defun clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 clojure-test-ns-segment-position))
         (after (subseq segments clojure-test-ns-segment-position))
         (test-segments (append before (list "test") after)))
    (mapconcat 'identity test-segments "/")))

(defun clojure-jump-to-test ()
  "Jump from implementation file to test."
  (interactive)
  (find-file (format "%s/test/%s.clj"
                     (locate-dominating-file buffer-file-name "src/")
                     (clojure-test-for (clojure-find-ns)))))

;;;###autoload
(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'interpreter-mode-alist '("jark" . clojure-mode))
(add-to-list 'interpreter-mode-alist '("cake" . clojure-mode))

(provide 'clojure-mode)
;;; clojure-mode.el ends here
