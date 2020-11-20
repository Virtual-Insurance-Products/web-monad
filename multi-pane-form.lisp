(in-package :web-monad)

(defstruct pane-stream mpane next)

(defun pane-stream (mpane next)
  (make-pane-stream :mpane mpane :next next))

(defun draw-pages (undrawn-items &key draw-this show-this)

  (labels ((draw-page (item current-page in-error show-page)
             (with-web-monad
               (value render) <- (value-and-render (melement (seq
                                                              val <- (pane-stream-mpane item)
                                                              (if in-error :error val) :== val-or-error

                                                              (unless (or in-error (> show-page 0))
                                                                (funcall show-this val-or-error current-page)) :== show

                                                              (unit (list show val-or-error)))
                                                    (<> :div
                                                       :style (unless (first (web-monad::value it))
                                                                (list :display "none"))
                                                       (dhtml (draw it)))))

               - (if (or (first value) (funcall draw-this current-page))
                     (mshow render)
                     (unit nil))

               (unit value)))

           (rec (items &optional (current-page 0) (show-page 0) in-error)
             (with-web-monad

               (show value)                             <- (draw-page items current-page in-error show-page)
               (if show current-page show-page)         :== showing-page
               (funcall (pane-stream-next items) value) :== next-value

               (if (equal (type-of next-value) 'pane-stream)
                   (rec next-value (1+ current-page) showing-page (or in-error
                                                                      (eq value :error)))
                   (unit (list (= showing-page current-page)
                               showing-page
                               (if in-error :error next-value)))))))

    (rec undrawn-items)))


(defun current-pane-stream-page (name)
  (list name "current-page"))

(defun mgroovy-pane-form (name items &key
                                       (next-button (lambda (n)
                                                      (declare (ignore n))
                                                      (msubmit (list name "next") "NEXT >")))
                                       (previous-button (lambda (n)
                                                          (declare (ignore n))
                                                          (msubmit (list name "previous") "< PREVIOUS"))))

  (with-web-monad

    current <- (mquery (list name "current"))

    (if current  (parse-integer current)  0) :== current

    max-progress <- (mquery (list name "max-progress"))

    (if max-progress (parse-integer max-progress) 0) :== max-progress

    next     <- (dont-render (funcall next-button current))
    previous <- (dont-render (funcall previous-button current))

    ;; This function will determine if we should show the given page.
    ;; We show it if we are in :error or if the page count is the one selected by the press of next or previous.
    (progn
      (let ((page
              (cond
                (next (if (eq value :error)
                          count
                          (1+ current)))
                (previous (1- current))
                (t current))))
        (= count page))) :== (show-this-page value count)

    (<= count (max current max-progress)) :== (draw-this-page count)

    ;; Draw all the pages. Returned is 't' if we are showing the last page, the number of the page being shown
    ;; and the final return from the monad stream.
    (last-item current-page value) <- (draw-pages items :show-this #'show-this-page :draw-this #'draw-this-page)

    (= current-page 0) :== first-item

    ;; now to get next and previous buttons to work we will need to update the current when one is pressed
    - (if first-item  (unit nil)     (funcall previous-button current-page))
    - (if last-item   (unit nil)     (funcall next-button current-page))

    - (mhtml (<> :input :type "hidden" :name (list name "current")
                       :value (s "~A" current-page)))

    - (mhtml (<> :input :type "hidden" :name (list name "max-progress")
                       :value (s "~A" max-progress)))

    - (set-monad-data (current-pane-stream-page name) current-page)

    (unit value)))
