;;; plugin.el --- Automatic fetching, installing and loading of Emacs modules according to widespread Emacs conventions.

;; Copyright (C) Guillaume Marceau, 2003

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


(require 'loop-constructs)
(require 'record)

;;; Code:
(eval-and-compile (require 'cl) (load "cl-macs"))

(defgroup plugin nil
  "Automatic fetching, installing and loading of Emacs modules according to widespread Emacs conventions."
  :group 'environment)

(defcustom plugin-installation-dir "~/elisp"
  "Default installation directory for downloader methods."
  :type 'directory
  :group 'plugin)

(defcustom plugin-download-urls '("ftp://ftp.rge.com/pub/gnu/elisp-archive/archive")
  "A list of URL for directories where to fetch Emacs modules from."
  :type '(repeat string)
  :group 'plugin)

(defcustom plugin-load-path nil
  "List of supplemental directories to look for files in, beside `load-path' and `plugin-installation-dir'."
  :type '(repeat directory)
  :group 'plugin)

(defcustom plugin-installed-modules nil
  "Modules that are loaded automatically at startup, and whenever you set this option."
  :type '(repeat (list symbol file))
  :group 'plugin
  :initialize 'custom-initialize-changed
  :require 'plugin
  :set '(lambda (symbol value)
          (let ((installed-features (mapcar (lambda (x) (if (listp x) (car x) x)) value)))
            ;; unload unwanted modules
            (mapcar '(lambda (x)
                       (if (and (not (member x value)) (featurep x))
                           (condition-case nil (unload-feature x) (error nil))))
                    installed-features)
            ;; set var
            (set-default symbol value)
            ;; load wanted modules
            (mapcar '(lambda (x)
                       (let ((f (if (listp x) (car x) x)))
                         (if (not (featurep f)) (plugin x))))
                    value))))
          
(defcustom plugin-module-download-methods '(plugin-download-feature)
  "List of function to call in order when trying to fetch a new module.
They should return nil on failure.  The functions
should have one argument, the feature name.  They should attempt
to fetch the module, but neither unpack it nor should they load
it."
  :type '(repeat function)
  :group 'plugin)

(record-def plugin-file (filename module autoload version has-extension compiled))

(defvar plugin-debug nil)

(defun plugin (feature-or-pair &optional disallow-partial-load)
  "Plugin is a `require' with an aggressive file search strategy.
Before loading a file, plugin adds the directory of the file
to `load-path'.  Thus the file being loaded can find its companions
files, if any.

FEATURE-OR-PAIR can also be a list of two elements.  The first element is the
symbol of the feature to load (as usual).  The second element should be
a filename to load it from.  This will bypass the search and make the loading
faster, unless loading directly from the file failed

Unless DISALLOW-PARTIAL-LOAD is non-nil, plugin will try to load an
\"-autoload\" file. This file should load the interface elements of
FEATURE, but delay the loading of its body.

When initially looking for a file, plugin looks across the load-path,
plugin-load-path and plugin-installation-dir (in this order) for :

module-name-VERSION/module-name-autoload.elc
then for module-name-VERSION/module-name-autoload.el
then for module-name-VERSION/module-name.elc
then for module-name-VERSION/module-name.el

then for module-name-autoload-VERSION.elc
then for module-name-autoload-VERSION.el
then for module-name-VERSION.elc
then for module-name-VERSION.el

then for module-name/module-name-autoload.elc
then for module-name/module-name-autoload.el
then for module-name/module-name.elc
then for module-name/module-name.el

then for module-name-autoload.elc
then for module-name-autoload.el
then for module-name.elc
then for module-name.el"
  (interactive "SPlugin feature by name : ")
  (let ((allow-autoload (not disallow-partial-load))
        (feature (if (listp feature-or-pair) (car feature-or-pair) feature-or-pair)))
    (if (featurep feature) (message "Plugin: `%s' already loaded" feature)
      (let ((filename (if (and (listp feature-or-pair)
                               (file-exists-p (cadr feature-or-pair)))
                          (cadr feature-or-pair)
                        (plugin-find-load-file feature allow-autoload))))
        (cond (filename
               (let ((plugin-load-directory (file-name-directory filename)))
                 (setq load-path (add-to-list 'load-path plugin-load-directory))
                 (message "Pluging %s ..." feature)
                 (if (string-match "-autoload" filename)
                     (load filename)
                   (require feature filename))
                 (message "Pluging %s ... done" feature)))

               ;; If we couldn't get a file from the filename given, try again
               ;; with the extensive search
               ((listp feature-or-pair) (plugin feature))

               (t (error "Error: could not find a file from which to load `%s'" feature)))))))

(defun plugin-make-autoload-file (target-file)
  "Scan TARGET-FILE for the magic marker ###autoload, and make a fast-load file.
The name of the target file will follow the search convention of
`plugin'.  Returns the nil if not autoload annotations were found,
otherwise returns the name the autoload file."
  (interactive "fMake an autoload file for file : ")
  (let ((parsed (plugin-parse nil target-file t)))
    (if (get-f autoload parsed) (error "`%s' already is an autoload file" target-file))
    (let* ((generated-autoload-file
            (concat (get-f module parsed)
                    "-autoload"
                    (if (get-f version parsed) (concat "-" (plugin-pp-version (get-f version parsed)))
                    ".el")))
           (should-kill-buffer (not (find-buffer-visiting generated-autoload-file))))
      (find-file generated-autoload-file)
      (if (eobp) (insert " ")) ;; update-file-autoloads complains if the file is empty
      (update-file-autoloads target-file)
      (goto-char 0)
      (let ((trivial (re-search-forward "\\`\\s-*\\'" nil t)))
        (set-buffer-modified-p (not trivial))
        (basic-save-buffer)
        (if should-kill-buffer (kill-buffer (current-buffer)))
        (byte-compile-file generated-autoload-file t)
        (if trivial (message "No autoload annotations found")
          (message "Wrote %sc" generated-autoload-file))  ;; put our message after basic-save-buffer's
        (if trivial nil generated-autoload-file))))) ;; return a value

(defun plugin-make-autoload-file-and-compile (target-file)
  (interactive "fMake an autoload file for file: ")
  (byte-compile-file target-file)
  (plugin-make-autoload-file target-file))

(defvar url-regexp "^[a-z]+://")


(defun plugin-install-get-file (feature-or-url)
  "Fetch a feature from the net via wget or `plugin-module-download-methods'.
If FEATURE-OR-URL is a symbol, the functions in `plugin-module-download-methods'
are called in order until on succeeds.  Returns null if the file could not be fetched."
  (let ((result
         (cond ((symbolp feature-or-url)
                ;; is feature
                (while-break ((cur plugin-module-download-methods))
                  (if (not cur) (break nil))
                  (let ((result (funcall (car cur) feature-or-url)))
                    (if result (break result)))
                  (setq cur (cdr cur))))

               ((string-match url-regexp feature-or-url)
                ;; is url
                (switch-to-buffer "*plugin-download*")
                (erase-buffer)
                (let ((error-code (call-process "wget" nil
                                                plugin-call-process-destination
                                                t
                                                feature-or-url
                                                "--directory-prefix"
                                                temporary-file-directory)))
                  (if (= error-code 0)
                      (let ((result (plugin-pickup-wget-saved-filename
                                     (file-name-nondirectory feature-or-url))))
                        (end-of-buffer)
                        (let ((msg (format "Plugin: download of `%s' successful." feature-or-url)))
                          (insert msg "\n")
                          (message msg))
                        (bury-buffer)
                        result))))

               (t (error "Bad url: %s" feature-or-url)))))
    (if (not result) (error "Error: could not fetch `%s'" feature-or-url))
    result))

(defun plugin-feature-of-filename (filename)
  (let ((base-filename (file-name-nondirectory filename)))
    (if (string-match "\\(^[a-z-]+\\)" base-filename)
        (let ((result (match-string 0 base-filename)))
          (if (= (aref result (- (length result) 1)) ?-)
              (substring result 0 (- (length result) 1))
            (intern result)))
      (error "Could not infer the feature name from the filename"))))

(defun plugin-install (feature &optional no-interaction)
  (interactive "SInstall feature by name : ")
  (plugin-install-do feature no-interaction))

(defun plugin-install-file (filename &optional no-interaction)
  (interactive "fInstall feature by filename : ")
  (plugin-install-do filename no-interaction))

(defun plugin-install-url (url &optional no-interaction)
  (interactive "sInstall feature from url : ")
  (plugin-install-do url no-interaction))

(defun plugin-companion-files (dir feature)
  (let ((quoted-feature-name (regexp-quote (symbol-name feature)))
        (result nil))
    (loop for cur in (directory-files dir) do
          (if (plugin-parse quoted-feature-name cur t)
              (setq result (cons cur result))))
    result))

(defun plugin-uninstall (feature &optional no-interaction)
  "Uninstalls a feature and remove its files.
Removes FEATURE from `plugin-installed-modules', unloads the feature
from memory and offer the delete all the files associated with the feature.
Goes ahead with asking for confirmations when NO-INTERACTION is non-nil."
  
  (interactive "SUninstall feature (name) : ")

  (let ((target-file (plugin-find-load-file feature t)))
    ;; Check if the undeletion is relevant
    (if (and (not (featurep feature))
             (not (member feature plugin-installed-modules))
             (not target-file))
        (error "`%s' is not installed" feature))
    
    ;; Unload
    (if (featurep feature) (unload-feature feature))

    ;; Remove from startup
    (if (member feature (mapcar (lambda (x) (if (symbolp x) x (car x))) plugin-installed-modules))
        (let ((trimmed-module-list
               (spinner ((cur plugin-installed-modules))
                 (cond ((not cur) nil)
                       ((or (eq feature (car cur))
                            (and (listp (car cur)) (eq feature (car (car cur)))))
                        (spin (cdr cur)))
                       (t (cons (car cur) (spin (cdr cur))))))))
          (customize-save-variable 'plugin-installed-modules trimmed-module-list)))
    
    ;; Delete files
    (if target-file
        (let ((target-file-dir (file-name-as-directory (expand-file-name (file-name-directory target-file))))
              (installation-dirs
               (mapcar '(lambda (f) (file-name-as-directory (expand-file-name f)))
                       (cons plugin-installation-dir (append plugin-load-path load-path)))))

          (if (member target-file-dir installation-dirs)
              ;; Delete a few files in a directory of many lisp files
              (let* ((companion-files (plugin-companion-files
                                       target-file-dir
                                       feature))
                     (count (length companion-files))
                     (companion-files-strs (mapcar (lambda (s) (format "%s, " s)) companion-files))
                     (companion-files-strs-with-and
                      (if (> count 1)
                          (reverse (append (list (car companion-files-strs) "and ") (cdr companion-files-strs)))
                        companion-files-strs))
                     (companion-files-str
                      (substring
                       (apply 'concat companion-files-strs-with-and)
                       0 -2))
                     (plurial (if (> count 1) "s" "")))
                
                (if (or no-interaction
                        (y-or-n-p (format "Remove %s file%s, %s ? " count plurial companion-files-str)))
                    (mapcar (lambda (f) (delete-file f)) companion-files)))

            ;; Delete a whole distribution directory
            (if (or no-interaction
                    (y-or-n-p (format "Remove ENTIRE DIR `%s'? " target-file-dir)))
                (plugin-call-process (format "Could not remove dir `%s'" target-file-dir)
                                     "rm" "-r" target-file-dir)))))
    (message "Done uninstalling `%s'" feature)))

(defun plugin-install-do (feature-or-filename-or-url &optional no-interaction)
  "Install a feature according to Emacs packaging conventions.
First, if the feature cannot be found localy, `plugin-install-do' offers to
download it using the `plugin-module-downloader-methods'.  Then, it
calls `plugin' on FEATURE-OR-FILENAME-OR-URL.  If the call to plugin
fails, `plugin-install' will `plugin-install' any dependencies of
FEATURE-OR-FILENAME-OR-URLS and try to plugin
FEATURE-OR-FILENAME-OR-URL again.  Then `plugin-install-do' will try to
generate an otherwise missing autoload file from the ###autoload
annotations contained in the main file, if any.  Finally, it gives the
option of adding FEATURE-OR-FILENAME-OR-URL to the customization
variable `plugin-installed-modules' list, such that
FEATURE-OR-FILENAME-OR-URL will be loaded next time Emacs starts.
Goes ahead with asking for confirmations when NO-INTERACTION is non-nil."
  (let* ((typ (cond ((symbolp feature-or-filename-or-url) 'is-symbol)
                    ((string-match url-regexp feature-or-filename-or-url) 'is-url)
                    ((file-exists-p feature-or-filename-or-url) 'is-file)
                    (t nil)))

         (feature (if (eq typ 'is-symbol) feature-or-filename-or-url
                    (plugin-feature-of-filename feature-or-filename-or-url)))

         (target-load-file (plugin-find-load-file feature t))

         (download (or (eq typ 'is-url)
                       (and (eq typ 'is-symbol)
                            (not target-load-file)
                            (or no-interaction
                                (y-or-n-p (format "`%s' could not be found localy.  Should we try to download it? "
                                                    feature))))))

         (target-or-archive-filename (cond ((eq typ 'is-file) feature-or-filename-or-url)
                             (download (plugin-install-get-file feature-or-filename-or-url))))

         (expanded-installation-dir (expand-file-name plugin-installation-dir)))
    
    ;; expand the archive (if needed) and copy it to the destination
    (when target-or-archive-filename
      (if (plugin-archive-type target-or-archive-filename)
          ;; an archive
          (unwind-protect
              (plugin-extract-to-dir
               feature
               target-or-archive-filename
               expanded-installation-dir)
            ;; unwinds :
            (if download (delete-file target-or-archive-filename)))

        ;; not an archive, just a lonely file
        (let ((destination-file-name 
               (expand-file-name (file-name-nondirectory target-or-archive-filename) 
                                 expanded-installation-dir)))

          (if (not (equal (expand-file-name target-or-archive-filename)
                          destination-file-name))
              (let ((ok-overwrite (or (not (file-exists-p destination-file-name))
                                      (y-or-n-p (format "File `%s' already exists. Overwrite? " 
                                                        destination-file-name)))))
                (copy-file target-or-archive-filename destination-file-name ok-overwrite)))))
      
      (setq target-load-file (plugin-find-load-file feature t))
      (if (not target-load-file) 
          (error "Error: Available archive did not provide an appropriate load file for feature `%s'" feature)))
    
    (put feature 'plugin-installing t)
    (unwind-protect
        (until ()
          (condition-case err
              (progn
                ;; Load the feature
                (plugin feature)
                
                ;; Show some initiative, and attempt to generate some autoload-magic by ourselves :
                (let ((main-file (plugin-find-load-file-record feature t)))
                  (if (and main-file (not (get-f autoload main-file)))
                      (let ((has-autoload-magic
                             (progn (find-file (get-f filename main-file))
                                    (unwind-protect (search-forward-regexp "^;;;###autoload" nil t)
                                      (kill-buffer (current-buffer))))))
                        (if has-autoload-magic
                            (if (or no-interaction
                                    (y-or-n-p (format (concat "`%s' has autoload annotations. "
                                                              "Generate an autoload file now? ") feature)))
                                (let ((auto-file (plugin-make-autoload-file (get-f filename main-file))))
                                  (if auto-file
                                      (progn (unload-feature feature)
                                             (plugin feature)
                                             (message "`%s' is now autoloaded" feature)))))))))
                
                ;; Save it for future boots
                (if (not (member feature plugin-installed-modules))
                    (if (or no-interaction
                            (y-or-n-p (format (concat "Should `%s' be loaded whenever Emacs boots? "
                                                      "(your .emacs will be modified) ") feature)))
                        (customize-save-variable 'plugin-installed-modules
                                                 (cons (list feature (plugin-find-load-file feature t))
                                                       plugin-installed-modules))))
                
                ;; Rejoice
                (message "Plugin: `%s' installed" feature)
                t)
            
            (file-error
             (if (string= "Cannot open load file" (nth 1 err))
                 (let ((missing (intern (nth 2 err))))
                   (cond ((get missing 'plugin-install)
                          (error "Error: circular dependency detected: `%s' needs `%s'"
                                 feature missing))
                         ((or no-interaction
                              (y-or-n-p (format "`%s' requires `%s', should we install it now? "
                                                feature missing)))
                          (plugin-install-do missing no-interaction) nil)
                         (t (message "Unfurfilled dependency: `%s'" missing))))
               (plugin-reraise err)))))
      (put feature 'plugin-installing nil))))

(defun plugin-reraise (err) (signal (car err) (cdr err)))
  

(defun plugin-pickup-wget-saved-filename (base-filename)
  (save-excursion
    (set-buffer "*plugin-download*")
    (end-of-buffer)
    (search-backward-regexp (concat "`\\("
                                    (regexp-quote temporary-file-directory)
                                    "/*"
                                    (regexp-quote base-filename)
                                    "\\(\\.[0-9]+\\)?\\)'"))
    (match-string 1)))



(defun plugin-download-feature (feature)
  "Download a feature from its symbol name FEATURE."
  (interactive "SDownload feature by name : ")

  (switch-to-buffer "*plugin-download*")
  (erase-buffer)

  (let* ((archive-names
          (map 'list '(lambda (fm) (format fm feature))
               '("%s.el.bz2"
                 "%s.el.gz"
                 "%s.el"
                 "%s.elc.gz"
                 "%s.elc"
                 "%s.tar.bz2"
                 "%s.tar.gz"
                 "%s.tgz"
                 "%s.tar")))
         (base-filename
          (while-break-n outer ((cn archive-names))
            (while-break ((curl plugin-download-urls))
              (if (not curl) (break nil))
              (let ((error-code (call-process "wget" nil
                                              plugin-call-process-destination
                                              t
                                              (plugin-append-directory (car curl) (car cn))
                                              "--directory-prefix"
                                              temporary-file-directory)))
                (if (= error-code 0)
                    (break-n outer (car cn))))
              (setq curl (cdr curl)))
            (setq cn (cdr cn)))))

    (if (not base-filename) nil
      (let ((result (plugin-pickup-wget-saved-filename base-filename)))
        (end-of-buffer)
        (let ((msg (format "Plugin: download of `%s' successful." feature)))
          (insert msg "\n")
          (message msg))
        (bury-buffer)
        result))))
    

(defun plugin-archive-type (filename)
  (while-break ((extensions '("tgz" "tar.gz" "tar" "zip" "gz" "bz2")))
    (if (not extensions) (break nil))
    (if (string-match (concat ".*\\." (car extensions) "\\(\\.[0-9]+\\)?$") filename)
        (break (car extensions)))
    (setq extensions (cdr extensions))))


(defvar plugin-call-process-destination '("*plugin-download*" . "*plugin-download*"))

(defun plugin-call-process (error-msg program &rest args)
  (if (not (= (apply 'call-process
                     (append (list program nil plugin-call-process-destination t) args))
              0))
      (error error-msg)))
  


(defun plugin-extract-to-dir (feature archive-filename destination-dir)
  (let* ((plugin-temp-dir (plugin-append-directory
                           temporary-file-directory
                           (make-temp-name (concat "plugin-download--" (symbol-name feature) "-"))))
         (archive-type (plugin-archive-type archive-filename))
         (trimmed-filename
          (if (string-match "\\(.*\\)\\.[0-9]+$" archive-filename)
              (match-string 1 archive-filename)
            archive-filename))

         (temp-filename (plugin-append-directory
                         plugin-temp-dir
                         (file-name-nondirectory trimmed-filename))))

          (make-directory plugin-temp-dir)
          (copy-file archive-filename temp-filename)

          (switch-to-buffer "*plugin-download*")
          (end-of-buffer)
          (unwind-protect
              (progn

                ;; Unpack :
                (cond ((not archive-type) (error "Not an archive"))

                      ((string= archive-type "bz2")
                       (plugin-call-process (format "Could not bunzip2 `%s'" temp-filename)
                                            "bunzip2" temp-filename))

                      ((string= archive-type "gz")
                       (plugin-call-process (format "Could not gunzip `%s'" temp-filename)
                                            "gunzip" temp-filename))
                      
                      ((string= archive-type "zip")
                       (plugin-call-process (format "Could not unzip `%s'" temp-filename)
                                            "unzip" temp-filename)
                       (delete-file temp-filename))
                      
                      ((or (string= archive-type "tgz")
                           (string= archive-type "tar.gz")
                           (string= archive-type "tar"))

                       (let ((is-gziped (not (string= archive-type "tar"))))
                         (plugin-call-process (format "Could not untar%s `%s'"
                                                      (if is-gziped "-z" "")
                                                      temp-filename)
                                              "tar"
                                              (if is-gziped "-xzf" "-xf")
                                              temp-filename
                                              "--directory"
                                              plugin-temp-dir))
                       (delete-file temp-filename))
                      
                      (t (error "Unknown archive type `%s'" archive-type)))
                
                ;; List files :
                (let* ((files
                        (plugin-filter
                         (lambda (x) (not (or (string-match "/.$" x) (string-match "/..$" x))))
                         (directory-files plugin-temp-dir t nil t)))
                       (file-cnt (length files)))

                  ;; Check that destination is OK :
                  (cond ((not (file-exists-p destination-dir))
                         (make-directory destination-dir))
                        ((not (file-directory-p destination-dir))
                         (error "Destination `%s' is not a directory" destination-dir)))

                  ;; Move files over :
                  (cond ((= file-cnt 0) (error "Unpacking archive `%s' did not lead any files" archive-filename))
                        ((= file-cnt 1)

                         (insert (shell-command-to-string
                                  (format "mv -v %s %s" (car files) destination-dir))))
                         
                        (t
                         (let ((destination-subdir (plugin-append-directory
                                                    destination-dir
                                                    (symbol-name feature))))
                           (make-directory destination-subdir)
                           (insert
                            (shell-command-to-string
                             (format "mv -v %s/* %s"
                                     plugin-temp-dir
                                     destination-subdir)))))))

                ;; Rejoice :
                (let ((msg (format "Plugin: successfully extracted `%s' to `%s'" feature destination-dir)))
                  (insert msg "\n")
                  (message msg)
                  (bury-buffer)))

            
            ;; unwinds :
            (shell-command-to-string (format "rm -rf %s" plugin-temp-dir)))))

(defun plugin-filter (pred lst)
  (cond ((not lst) nil)
        ((funcall pred (car lst))
         (cons (car lst) (plugin-filter pred (cdr lst))))
        (t (plugin-filter pred (cdr lst)))))
                   


(defvar plugin-autoload-regexp "\\(-autoload\\)?")
(defvar plugin-version-regexp "\\([-.]\\([0-9]+\\(\\.[0-9]+\\)*\\)\\)?")
(defvar plugin-extension-regexp "\\(\\.elc?\\)?")

(defun plugin-parse-version (version-string)
  (mapcar '(lambda (x) (string-to-int x)) (split-string version-string "\\.")))

(defun plugin-pp-version (version-list)
  "Pretty print the version numbers in VERSION-LIST to a string."
  (until (rtn) (if (not version-list) rtn
                 (setq rtn (concat rtn (if rtn ".") (int-to-string (car version-list))))
                 (setq version-list (cdr version-list))
                 nil)))


(defun plugin-append-directory (base extra)
  (concat (file-name-as-directory base) extra))

(defun plugin-parse (quoted-module-name filename allow-autoload)
  (if (string-match (concat (if quoted-module-name (concat "^" quoted-module-name))
                            plugin-autoload-regexp
                            plugin-version-regexp
                            plugin-extension-regexp "$") filename)
      (let* ((module (substring filename 0 (or (match-beginning 1)
                                               (match-beginning 3)
                                               (match-beginning 5)
                                               (length filename))))
             (autoloaded (if (match-string 1 filename) t))
             (has-extension (if (match-string 5 filename) t))
             (compiled (and has-extension (string= (match-string 5 filename) ".elc")))
             (version (if (match-string 3 filename)
                          (plugin-parse-version (match-string 3 filename)))))
        (if (and autoloaded (not allow-autoload)) nil
          (make plugin-file filename module autoloaded version has-extension compiled)))))

(defun plugin-version-less-than (first second)
  (cond ((not first) second)
        ((not second) first)
        ((< (car first) (car second)) t)
        ((= (car first) (car second)) (plugin-version-less-than
                                       (cdr first) (cdr second)))
        (t nil)))


(defun plugin-keep-best (current candidate allow-dir allow-version)
  (cond ((not candidate) current)
        ((and (not allow-dir) (not (get-f has-extension candidate))) current)
        ((and (not allow-version) (get-f version candidate)) current)
        ((not current) candidate)
        ((and (not (get-f version current)) (get-f version candidate)) candidate)
        ((and (get-f version current) (get-f version candidate)
              (plugin-version-less-than (get-f version current) (get-f version candidate)))
         candidate)
        ((and (not (get-f autoload current)) (get-f autoload candidate)) candidate)
        ((and (not (get-f compiled current)) (get-f compiled candidate)) candidate)
        (t current)))


(defun plugin-find-load-file-in-dir (dir quoted-module-name recurse allow-autoload)
  (if (not (file-exists-p dir)) nil
    (let (best)
      (loop for cur in (directory-files dir) do
            (if plugin-debug
                (save-excursion
                  (set-buffer (get-buffer-create "*plugin*"))
                  (insert (format "%s / %s : %s\n" dir best cur))))
            (setq best (plugin-keep-best best (plugin-parse
                                               quoted-module-name
                                               cur
                                               allow-autoload)
                                         recurse recurse)))
      (cond ((not best) nil)
            ((and recurse (not (get-f has-extension best))
                  (file-directory-p (plugin-append-directory
                                     dir
                                     (get-f filename best))))
             (or (plugin-find-load-file-in-dir (plugin-append-directory
                                                dir
                                                (get-f filename best))
                                               quoted-module-name nil allow-autoload)
                 (error "Found the plugin directory `%s', but did not find an appropriate load file in it. Use plugin-install-file to select one."
                        (plugin-append-directory dir (get-f filename best)))))
            (t (with-f filename best (plugin-append-directory dir (get-f filename best))))))))
                       

(defun plugin-find-load-file-in-dirs (dirs quoted-module-name allow-autoload)
  (let (best)
    (loop for f in dirs do
          (setq best (plugin-keep-best best (plugin-find-load-file-in-dir
                                             f quoted-module-name
                                             t allow-autoload)
                                       nil t)))
    best))

(defun plugin-find-load-file-record (module allow-autoload)
  (plugin-find-load-file-in-dirs
   (append load-path plugin-load-path
           (and plugin-installation-dir (list plugin-installation-dir)))
   (regexp-quote (symbol-name module))
   allow-autoload))

(defun plugin-find-load-file (module allow-autoload)
  (let ((best (plugin-find-load-file-record module allow-autoload)))
    (and best (get-f filename best))))

(defun plugin-feature-installed-entry-equalp (a b)
  (let ((sa (if (symbolp a) a (car a)))
        (sb (if (symbolp b) b (car b))))
    (eq sa sb)))

(provide 'plugin)

(provide 'plugin)

;;; plugin.el ends here
