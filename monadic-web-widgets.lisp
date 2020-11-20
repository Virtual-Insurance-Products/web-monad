
(in-package :web-monad)




;; FORMATTERS

;; Various things can use formatters to constrain, parse and present their values


;; formatters...
;; I could make an option to have display formatters and result formatters...

;; a formatter returns a list of two values: display value and return value
;; this would be really useful for dealing with dates and currencies...
(defun strip-space-formatter ()
  #'(lambda (value)
      (awhen (strip-leading-and-trailing-space value)
        (list it it))))

;; I could add in replacements too I suppose
(defun regex-formatter (regex)
  #'(lambda (value)
      (let ((value (strip-leading-and-trailing-space value)))
        (aif (scan regex value)
             (list value value)
             (list value :error)))))

;; !!! NOTE - I did have a super-date-parser function which was
;; !!! intended to be able to parse lots of things (including things
;; !!! like 'next tuesday' etc), but it doesn't work because the
;; !!! parser monad has not yet stabilised. That's still on my list of
;; !!! things to sort out. I was going to implement parsec.
(defun date-formatter (&key (minimum 0) (maximum (parse-date "1/1/3000")) (date-parser #'parse-date))
  #'(lambda (value)
      (let ((v (cond ((stringp value)
                      (aif (ignore-errors
                             (funcall date-parser (strip-leading-and-trailing-space value)))
                           (when (and (>= it minimum)
                                      (< it maximum))
                             it)))
                     ((numberp value) value)
                     (t nil))))
        (if v
            (list (formatted-date v) v)
            (list value :error)))))

(defun uk-postcode-formatter ()
  #'(lambda (postcode)
      (if (scan "^[A-Z\\d]+\\s*[A-Z\\d+]+$" (string-upcase postcode))
          (let ((postcode (regex-replace "([^\\s])(\\d[A-Z]{2})$"
                                         (string-upcase postcode)
                                         "\\1 \\2")))
            (list postcode postcode))
          (list postcode :error))))



(defun number-formatter (&key minimum maximum minimum-digit-count)
  #'(lambda (value)
      (let ((v (cond ((stringp value)
                      (ignore-errors (parse-rational (strip-leading-and-trailing-space value))))
                     ((rationalp value) value)
                     (t nil))))
        (if (and v
                 (or (not minimum) (>= v minimum))
                 (or (not maximum) (<= v maximum)))
            (list (if minimum-digit-count
                      (zero-pad v minimum-digit-count)
                      (s "~A" v))
                  v)
            (list value :error))
        )))

;; !!! I haven't made these 
(defun currency-formatter (&key display-currency-symbol)
  #'(lambda (value)
      (let ((v (cond ((stringp value)
                      (ignore-errors
                        (parse-rational (regex-replace-all "[^\\d\\.\\-]" value ""))))
                     ((rationalp value) value)
                     (t nil))))
        (if v
            (list (if display-currency-symbol
                      (s "~A~/dollar/" #\Pound_Sign v)
                      (s "~/dollar/" v))
                  v)
            (list value :error)))))

(defun non-empty-formatter (&optional (empty-val ""))
  #'(lambda (value)
      (awhen (strip-leading-and-trailing-space value)
        (if (equal it empty-val)
            (list it :error)
            (list it it)))))


(defun minimum-length-formatter (length)
  #'(lambda (value)
      (awhen (strip-leading-and-trailing-space value)
        (if (< (length value) length)
            (list it :error)
            (list it it))))
  
  )

;; I suppose I should define a do nothing formatter. (@ list ?1 ?1)


;; SIMPLE WIDGETS

;; TODO - AJAX stuff; formatting options; type checking and error notification; alternative renderings
(defun minput (name &key default value class style (formatter (strip-space-formatter))
                      pattern autofocus
                      always-replace-value
                      size                     ; character width
                      type min max ;; these are useful for range widgets...
                      readonly
                      placeholder
                      spellcheck
                      autocomplete ; pass "off" to disable that
                      autocorrect ; pass off to disable
                      (no-dynamic-update t) ; I think it's annoying having this by default. There are many circumstances where you don't want to do this. I just hope that changing this default doesn't cause problems though
                      mandatory
                      ;; only do the dynamic update stuff if input string matches this pattern
                      (dynamic-update-pattern nil)
                      validate ; pass t (or monad(t)) to trigger validation and display update...
                      (error-style '(:background-color "#f99")))

  (with-web-monad
    ;; we can override the value using that parameter to make this ignore the posted value
    query-value <- (mquery name :default default)
    (or value 
        (if (and placeholder
                 (equal placeholder query-value))
            ""
            query-value))
    :== value

    (when value
      (funcall formatter value)) :== formatted

    validate <- (as-web-monad validate)

    (unless (or (not dynamic-update-pattern)
                (scan dynamic-update-pattern query-value))
      (setf validate nil
            no-dynamic-update t))
    
    ;; (break)
    
    ;; nifty thing to update my value when an AJAX event happens...
    ;; I might *possibly* have to be careful here in case weird things happen, but I reckon we should be ok.
    - (if (and formatted
               (or validate (not no-dynamic-update)))
          ;; don't replace if it changed since we started handling the request
          ;; this nicely and completely solves that problem (would be nice to rate limit as well - see continuation framework for easy way to do this)
          (ajax-js (if always-replace-value
                       (js.set-slot-value name 'value (first formatted))
                       `(when (= (slot-value ($e ,(query-parameter-name name)) 'value)
                                 ,query-value)
                          ,(js.set-slot-value name 'value (first formatted)))))
          (unit nil))

    - (if (or validate (not no-dynamic-update))
          (if (eq (second formatted) :error)
              ;; !!! I need a way to do this properly based on the parameter above
              (ajax-js (js.set-slot-value name 'style.background-color "#f99"))
              (ajax-js (js.set-slot-value name 'style.background-color "")))
          (unit nil))

    ;; I should highlight errors too...
    ;; that will be a simple question of a bit more ajax-js
    
    - (mhtml (<> :input :name (query-parameter-name name)
                 :id (query-parameter-name name)
                 :readonly (when readonly "readonly")
                 :pattern pattern :autofocus (when autofocus "autofocus")
                 :type type
                 :min min :max max
                 :size size
                 :spellcheck spellcheck
                 :autocomplete autocomplete
                 :autocorrect autocorrect
                 :placeholder placeholder ; !!! This is not that widely supported so I probably need to re-implement it somehow...
                 :class (if (eq (second formatted) :error)
                            (if class
                                (s "~A error" class)
                                "error")
                            class)
                 :style (append style
                                (when (and placeholder (not value))
                                  (list :color "#999"))
                                (when (eq (second formatted) :error) error-style))
                 :value (if value (first formatted) (or placeholder ""))
                 (output-html-events)))

    ;; to implement the placeholder when we don't have support for that attribute...
    ;; (I could try and check whether we do or not, but I don't know how)
    ;; the only problem with this is that the placeholder will be returned as the value. You'd better hope it isn't a valid value. This causes problems with the focus/blur anyway.
    ;; on the whole this isn't a perfect solution, but it will work provided the formatter doesn't accept the placeholder
    - (if placeholder
          (mprogn
            (event-handler name :onfocus `(when (= this.value ,placeholder)
                                            (setf this.value ""
                                                  ;; This will break things if we have a different color set. I wonder how I can fix that?
                                                  this.style.color ,(or (second (member :color style))
                                                                        ""))))

            (event-handler name :onblur `(when (= this.value "")
                                           (setf this.value ,placeholder
                                                 this.style.color "#999")))

            )
          (unit nil))

    ;; !!! we could stop the placeholder from being returned here
    (unit (if value 
              (second formatted)
              (when mandatory
                :error)))))

(defun mtextarea (name &key default value style (formatter (strip-space-formatter))
                  readonly
                  dont-update-self
                  class
                  (error-style '(:background-color "#f99")))
  (with-web-monad
    ;; we can override the value using that parameter to make this ignore the posted value
    value <- (if value (unit value)
                 (mquery name :default default))
    (when value
      (funcall formatter value)) :== formatted

      ;; nifty thing to update my value when an AJAX event happens...
      ;; I might *possibly* have to be careful here in case weird things happen, but I reckon we should be ok.
      ;; thiw will cause problems sometimes - particularly if we're trying to change the value of this
    - (if (and formatted (not dont-update-self))
          (ajax-js (js.set-slot-value name 'value (first formatted)))
          (unit nil))

    - (if (eq (second formatted) :error)
          ;; !!! I need a way to do this properly based on the parameter above
          (ajax-js (js.set-slot-value name 'style.background-color "#f99"))
          (ajax-js (js.set-slot-value name 'style.background-color "")))
    
    ;; I should highlight errors too...
    ;; that will be a simple question of a bit more ajax-js
    
    - (mhtml (<> :textarea :name (query-parameter-name name)
                 :id (query-parameter-name name)
                 :readonly (when readonly "readonly")
                 :style (append style
                                (when (eq (second formatted) :error) error-style))
                 :class class
                 (output-html-events)
                 (dhtml (when value
                          (let ((value (cl-ppcre:regex-replace-all
                                        ">" (cl-ppcre:regex-replace-all
                                             "<" (cl-ppcre:regex-replace-all "&" (first formatted) "&amp;")
                                             "&lt;") "&gt;")))
                            (format com.gigamonkeys.html::*html-output* "~A" value))))))
    

    (unit (when value (second formatted)))))

;; I want a function to return whether a post was done or not...
(defun mpost-p ()
  (make-web-monad :handler #'(lambda (+request)
                               (make-web-thunk :value (and +request
                                                           (not (consp +request))
                                                           ;; I'm not sure that I should do this like this.
                                                           (equal (s "~A" (request-method +request)) "POST"))))))

(defun mhost-uri ()
  (with-web-monad
    request <- (mrequest)
    (unit (when request
            (format nil "~A/" (net.uri::render-uri
                               (net.uri::merge-uris #u "/" (request-uri request))
                               nil))))))

(defun mrequest-uri ()
  (with-web-monad
    request <- (mrequest)
    (unit (when request (s "https://abelonline.co.uk~A" (request-raw-uri request))))))

;; this excludes the post handler part...
(defun mbase-uri ()
  (with-web-monad
    uri <- (mrequest-uri)
    (unit (regex-replace "\\/p$"
                         ;; remove query parameters
                         (regex-replace "\\?.*$" uri "")
                         ""))))

(defun current-monad-id ()
  (with-web-monad
    uri <- (mbase-uri)
    (unit (regex-replace ".*/" uri ""))))

;; !!! I should add events...
(defun mcheckbox (name &key default (force-value nil value-supplied-p) (events-name name) class style required validate clear formatter)
  (declare (ignore formatter))
  (flet ((post-p (+request)
           (and +request
                (or (and (not (consp +request))
                         ;; I'm not sure that I should do this like this.
                         (equal (s "~A" (request-method +request)) "POST"))
                    (and (consp +request)
                         (find :post +request :key #'first))))))
    (with-web-monad
      value <- (if clear (unit nil) (mquery name))
      validate <- (as-web-monad validate)
      #+nil(if validate
            (if (and required validate (not value))
                ;; !!! I need a way to do this properly based on the parameter above
                (ajax-js (js.set-slot-value name 'style.outline "4px solid #f99"))
                (ajax-js (js.set-slot-value name 'style.outline "")))
            (unit nil))

      - (mhtml (<> :input :type "checkbox" :name (query-parameter-name name)
                   :value "yes" ; the value isn't really important - only whether or not it's checked. Any true value will ensure that
                   :class class
                   :style (if (and required
                                   validate
                                   (not value)
                                   (post-p +request))
                              (append style '(:outline "4px solid #f99"))
                              style)
                   :checked (cond (value-supplied-p force-value)
                                  (clear nil)
                                  ((post-p +request) value)
                                  (t default))
                   ;; !!! NEW (2012-03-20) - a protocol for handling events targeted at someone else!
                   ;; (in addition to normal ones)
                   (let ((name events-name))
                     (output-html-events))))
      
      ;; The actual value returned will just be t or nil unless a default is specified, in which case it will be 
      ;; This is surprisingly awkward to handle. If we're responding to GET then we just return the default. Otherwise we return the POSTed value...
      ;; EXCEPT that if there is a default we return that if the checkbox was checked
      (make-web-monad :handler #'(lambda (+request)
                                   (make-web-thunk :value (if (post-p +request)
                                                              (if value (or default t))
                                                              default)))))))


(defun msubmit (name label &key style class)
  (with-web-monad
    - (mhtml (<> :input :type "submit" :name (query-parameter-name name) :value label
                 :style style
                 :class class
                 (output-html-events)))
    (mquery name)))

;; This is intended to look like a normal link (or however it's styled) but behave like a submit button. It's a bit of a trick. Submit buttons are now much more useful.
;; I would have thought there would be a better way of doing this in HTML.

;; This gives a possible reasonable way of handling navigation links (like at the top). I would have to wrap the top nav thing in the form in ABEL page, but that should be fine.
;; Because we can use redirects we wouldn't create a new monad handler every time the top nav was drawn - only every time the item was clicked, which isn't too bad. Could be an easy solution to the problem.
;; The only problem with this (and it's a big one) is that you can't middle click for a new tab. I wonder if I can fix that. It works with form submit buttons.
;; I could do a load of styling things to try and make submit buttons not look like such. Don't know if it will always work. It's also limited.
;; OR I could use button, which can have (almost) arbitrary content. It still doesn't stop it from being styled though. I would have to explicitly get rid of all the button styles.
;; there are trade offs. On the other hand, if I'm explicitly styling things ANYWAY it hardle matters.
;; The thing below will have its uses (sometimes we don't want middle click). Also, when used in an AJAX mode we don't want things to trigger form POSTS but I think I'll need another set of buttons for that.
(defun msubmit-link (name content)
  (let ((id (js-next-window-id)))
    (with-web-monad
      - (mhtml
          (:input :id id :value "1" :type "hidden")
          (:span :onclick (js `(let ((input (document.get-element-by-id ,id)))
                                 (setf (slot-value input 'name)
                                       ,(query-parameter-name name))
                                 (input.form.submit)))
                 :style (css :cursor "pointer")
                 (if (functionp content)
                     (funcall content)
                     (html content))))
      (mquery name))))

(defun select-has-value (value posted multiple)
  (if multiple
      (member value posted :test #'equal)
      (equal value posted)))

;; the values are a list of (label object) pairs. This means that the selector allows one to select between a series of arbitrary objects - not just text strings
;; this function will handle switching between what the browser send and those objects
;; !!! NOTE - the trigger on change is due to be deprecated in favour of a more general mechanism for putting in event handlers. It will use the whole AJAX scheme of things.
(defun mselect (name values &key default style trigger-on-change readonly (test #'equal) noescape value multiple class
                              disabled)
  ;; You can supply different tests for equality...
  (with-web-monad
    ;; first see if there's a posted values
    posted <- (mquery name)
    ;; then draw the thing...
    - (mhtml (<> :select :style style :name (query-parameter-name name)
                 :onchange (when trigger-on-change "this.form.submit()")
                 :multiple multiple
                 :class class
                 :disabled disabled
                 (output-html-events)
                 (dhtml
                   (loop for i from 1
                      for (label object) in values
                      for this-value = (s "_~A" i)
                      do (html (<> :option :value this-value
                                  :disabled (when readonly t)
                                   :selected (when (or (and value
                                                            (funcall test value object))
                                                       (and (not value)
                                                            (equal posted this-value))
                                                       (and (not posted)
                                                            (not value)
                                                            (funcall test default object))) 
                                               "selected")
                                   (if noescape
                                       (dhtml (:noescape label))
                                       (dhtml label))))))))

    - (mmapcar-i :web-monad
                 (lambda (i x)
                   (if (third x)
                       (event-handler name :onchange
                                      `(if (= this.value ,(s "_~A" (1+ i)))
                                           ,(third x)))
                       (unit nil)))
                 values)
    
    ;; It's a shame we seem to have to do this looping again. There must be a way of avoiding this.
    ;; I think maybe I could lexically bind a function which would do the outputting or something and call it twice.
    (unit 
     (let ((result (loop for i from 1
                    for (nil object) in values
                    for this-value = (s "_~A" i)
                    when (or (select-has-value this-value posted multiple)
                             (and (not posted)
                                  (funcall test default object)))
                      collect object)))
       (if multiple 
           result
           (first result))))))


;; This is a button. It doesn't do a form submit like an msubmit. It's just for ajax stuff.
;; If you do want to trigger a submit with it then you need to rig an event handler to do that...
;; !!! Obviously there need to be more like this...
;; this is just a test of the general principle.
(defun mbutton (name label &key style disabled class action)
  (let ((button
          (mhtml (<> :button :type "button"
                             :id name
                             :class class
                             :style style
                             :disabled (when disabled "disabled")
                             (output-html-events)
                             label))))
    (if action
        (bind (event-handler name :onclick action)
          (lambda (x)
            (declare (ignore x))
            button))
        button)))


;; I can then build up from there to recreate the shiny buttons which respond better to being clicked.
;; the problem with using them as submits, of course, would be lack of middle click to open new tab. shame.





;; A date widget...

;; (I wonder if I should make this clever so that it changes the available days selections?)
;; !!! unused

(defun parsed-date (string-or-number)
  (if (stringp string-or-number)
      (parse-date string-or-number)
      string-or-number))

(defun days-in-month (month-number year)
  (flet ((num (x)
           (if (stringp x)
               (parse-integer x)
               x)))
    (if (member (num month-number) '(9 4 6 11) :test #'=)
        30
        (if (= (num month-number) 2)
            (if (is-leap-year (num year))
                29 28)
            31))))







;; CONDITIONAL VISIBILITY

;; I have a feeling this is incredibly useful. It might solve a lot of awkward problems with showing popup windows etc (see the check boxes in the bundling application process for example)
;; !!! INVESTIGATE THE UTILITY OF THIS...

;; If I wanted the ability to have windows which pop up then I could put a hidden field which determines the visibleness of the window. If the page is reloaded the visibility would be preserved because the hidden value would be posted back. Can this be extended to deal with a list of windows? It probably can.
;; In this instance we have a multi value widget and an item list (in fact I might not want to use the multi value thing) which can be used to preserve, on the client, which windows are visible.
;; it might be necessary to extend the item list if we need to preserve the type of the window too.
;; This should provide a simple general mechanism for dynamically loaded content. There are things to bear in mind - any interaction which triggers AJAX calls will send ALL parameters back to the server; in replying to AJAX calls the server must evaluate the entire monad each time.

;; this means that we will need to be careful to take any expensive evaluation outside the monad and lexically close it. This is easy enough though. 

;; Isn't this more general than conditional visibility? It's a useful way of making things which redraw under certain conditions

;; This needs more parameters to do the AJAX update
;; I think this should be deprecated. It doesn't really add anything useful.
(defun conditionally-visible (name monad &key (visibility (with-web-monad (unit t)))
                              (update-event :onchange)
                              (update-object nil)
                              ;; invisible can mean just don't render, but you can avoid calling the monad at all if required...
                              (invisible #'dont-render))
  (with-web-monad
    visible <- visibility
    - (if* (unit update-object)
           (if* (ajax-event update-object update-event)
                (redraw name (if visible monad
                                 (funcall invisible monad)))))
    (draw-context name
                  (if visible
                      monad
                      ;; we can just not render it - we don't have to bother with display: none since we're going to go back to the server anyway
                      (funcall invisible monad)))
    ))



;; !!! Unused
(defun client-side-counter (name initial-count &key increment)
  (if increment
      (with-web-monad
        count <- (mquery name)
        (+ (parse-integer (s "~A" count)) increment) :== count
        - (mhtml (<> :input :name name
                     :type "hidden" :value count))
        (unit count))
      (mhidden-field name :default initial-count)))


;; This is used to put a list of item id components on the client. That way we can add or remove items
(defun item-list (name &key default add remove)
  (with-web-monad
    items <- (mquery name :default default)

    (split " " items) :== split
    
    (loop for a in (if add
                       (append split
                               (list (js-next-window-id)))
                       split)
       unless (equal a remove)
       collect a)
    :== items

    ;; Now render the values in a hidden field
    - (mhtml (<> :input :type "hidden" :name name :value (string-list items " ")))   

    (unit items)))

;; how do I remove individual items?
;; If I can figure out how to remove individual items then I might have a general solution for making these sort of multi wotsits quite straightforwardly
;; OTOH I could handle removal differently - I could do it all on the client side and not build it into this multi-value-input at all
;; I could just make another monad and include it in the field type. It would be quite simple. If the field is represented as a single element (which it isn't necessarily) then removing that is easy.
;; then I would have to update the client side counter, but that wouldn't be difficult.
;; the remover would have to have knowledge of the multi-value-input obviously, but that's not a problem I don't suppose.
;; I think client side removal would be better really. The other thing I could do is to wrap the fields in elements here, but that would complicate this thing

(defun multi-value-input (name &key default
                          ;; class  ; unused
                          (add-button-label "Add item")
                          (delete-button-postfix "delete-item")
                          (field #'minput))
  (with-web-monad
    (flet ((draw-inner (&key add remove)
             (seq
               items <- (item-list (list name "items") :default "1" :add add :remove remove)

               ;; I need this to handle defaults properly unfortunately...
               items-without-removing <- (dont-render (item-list (list name "items") :default "1" :add add))
               
               (mmapcar :web-monad
                        (lambda (item value)
                          (seq
                            value <- (funcall field (list name item) :default value)
                            ;; You have to draw a delete button if one is required
                            delete <- (if* (ajax-event (list name item delete-button-postfix) :onclick)
                                           (unit item))
                            
                            (unit (list value delete))))
                        items
                        (loop for item in items-without-removing
                           for value in default
                           unless (equal item remove)
                           collect value)))))
      
      (seq
        values <- (draw-context (list name "draw-context")
                                (draw-inner))

        - (mmapcar :web-monad
                   (lambda (x)
                     (if (second x)
                         (redraw (list name "draw-context")
                                 (draw-inner :remove (second x)))
                         (unit nil)))
                   values)
        
        - (mbutton (list name "add") add-button-label)
        - (if* (ajax-event (list name "add") :onclick)
               (redraw (list name "draw-context")
                       (draw-inner :add t)))

        (mapcar 'first values)
        :== values
        
        (if (find :error values)
            (unit :error)
            (unit values))))))




;; this is a generalised popup dialog. In order for it to be styled there needs to be some CSS defined somewhere

;; I now know how to do all that sort of thing much better
(defun popup-dialog (name content &key open close)
  (with-web-monad

    open <- (as-web-monad open)
    close <- (as-web-monad close)
    
    (draw-context name (if open
                           (dimmer (draw-context (list name "inner")
                                                 content
                                                 :class "pop"))
                           ;; needed to yield a value
                           (dont-render content))
                  :class "dialog"
                  :redraw (or open close))))





;; editable images - much simpler way of uploading pictures and it automatically scales the image to within certain constraints.
;; This could be extended a lot to allow changing of the image etc, but this is very useful for now...

;; for the time being this needs to be included on any page which uses the editable image thing in order to make paste work.
(defun image-paste-handler ()
  (mhtml (:script "
document.onpaste = function(event) {
if(pasteTarget) {
  var items = event.clipboardData.items;
  // console.log(JSON.stringify(items));
  var blob = items[0].getAsFile();
  var reader = new FileReader();
  reader.onload = function(event){
     pasteTarget(event.target.result);};
  reader.readAsDataURL(blob);
}}

")))


;; the value can be any kind of URI including data ones, which is what we'll get by pasting or dragging things here...
;; NOTE - the width and height can be set independently of the max. It would be bad to set both though.
;; the image will be scaled to fit within the max dimensions. 
;; all that's left to do now is to implement storing of these things on the server. This is very slick
;; maybe some optimisation of the generated HTML

;; NOTE - it might be useful to have a flag to stop this from scaling up - only down
(defun editable-image (name &key default width height style (max-width 300) (max-height 200) (upscale nil))
  (with-web-monad

    value <- (mhidden-field name :default default)
    - (mhtml (<> :img :src value
                 :id (list name "img")
                 :width width :height height
                 :style (append style (list :cursor "pointer"))
                 ;; we only want the events for the img part here...
                 (let ((name (list name "img")))
                   (output-html-events))))

    - (event-handler (list name "img") :onmouseover '(setf this.style.box-shadow "0 0 15px #aaf"))
    - (event-handler (list name "img") :onmouseout '(setf this.style.box-shadow ""))

    ;; there ought to be a way not to have to duplicate this in the HTML, but...
    
    ;; maybe I could just use js-next-window-id to generate a named function here. This doesn't need to be persisted in any way does it? It's purely client side logic (I think)
    ;; (that would leave garbage functions on the client, which isn't ideal)
    ;; now all I need to do is modify this to scale the image using a canvas and we're done.
    `(setf paste-target (lambda (data)
                          (let* ((e ($e ,(query-parameter-name (list name "img"))))
                                 (h ($e ,(query-parameter-name name)))

                                 (i (document.create-element "img"))
                                 (c (document.create-element "canvas"))
                                 (d (c.get-context "2d")))
                            

                            (setf i.onload (lambda (event)
                                             ;; figure out the size for the canvas...
                                             (let* ((scale1 (/ ,max-width i.width))
                                                    (scale2 (/ ,max-height i.height))
                                                    (scale (let ((scale (if (> scale1 scale2) scale2 scale1)))
                                                             ,(if upscale
                                                                  'scale
                                                                  '(if (< scale 1) scale 1)))))
                                               
                                               (setf c.width (* i.width scale)
                                                     c.height (* i.height scale))
                                             
                                             ;; paste it into the canvas
                                             ;; !!! Calculate the proper height
                                             (d.draw-image i 0 0 c.width c.height)
                                             (setf e.src (c.to-data-u-r-l))
                                             (setf h.value (c.to-data-u-r-l)))

                                             ;; then trigger the onchange handler of the hidden input field if there is one...
                                             (let ((a (slot-value ($e ,(query-parameter-name name))
                                                                  'onchange)))
                                               (a))))
                            
                            
                            (setf i.src data)
                            
                            ;; (setf e.src data)
                            ;; store it here too so it gets sent back to us
                            ;; (setf h.value data)
                            )))
    :== paste-handler
    
    ;; !!! This has to do the jiggery pokery for scaling etc
    - (event-handler (list name "img") :onmouseover paste-handler)

    - (event-handler (list name "img") :ondrop paste-handler) ;; all this does is set the paste-target variable ready to receive. Maybe I should restructure this a little.
    
    - (drop-zone-highlight (list name "img"))
    
    ;; This wires up the drop handler - I should probably generalise this to handle things other than files if that's possible. I suspect it is. Then I could drag images from web pages probably.
    ;; I need to play with this DnD stuff a bit - it's really cool
    - (event-handler (list name "img") :ondrop "var r = new FileReader(); r.onload=function(event) {pasteTarget(event.target.result);};E=event;r.readAsDataURL(event.dataTransfer.files[0])")
    
    ;; then we need to start equipping the image with event handlers to make it editable
    ;; I probably need some constraint parameters too so as to give maximum dimensions to this
    
    (unit value)))







;; Some general drag and drop stuff...

;; maybe I should abstract out the DnD stuff? If there's a useful way to do that. Although it might be quite straightforward to use naked anyway. It would be trivial to make a (drag-drop-highlight name) function...
;; actually, drop-zone-highlight
;; I could do this by assigning classes, but never mind that for now.
;; This makes something a drop zone, although it doesn't make it respond to the drop. That is application specific.
(defun drop-zone-highlight (name &key (highlight '(setf this.style.box-shadow "0 0 15px #66f"))
                            (unhighlight '(setf this.style.box-shadow "")))
  (with-web-monad
    (mprogn
      (event-handler name :ondragover `(progn
                                         (event.stop-propagation)
                                         (event.prevent-default)
                                         ,highlight))
      (event-handler name :ondragleave unhighlight)
      (event-handler name :ondrop `(progn
                                     (event.stop-propagation)
                                     (event.prevent-default)
                                     ,unhighlight)))))


;; ALSO we could allow direct uploading of CSV data that way, although people would probably mostly try and upload Excel spreadsheets and not know the difference, so it might not be that good.
;; We could allow drag and drop of CSV data from a spreadsheet directly onto a page element too.
(defun ajax-drag-drop (object data-type &key handler)
  (with-web-monad
    - (event-handler object :ondrop `(when (event.data-transfer.get-data ,data-type)
                                       ,(js.ajax-event object :ondrop :value `(event.data-transfer.get-data ,data-type) :handler handler)))
    ;; then we return the value of whether or not the event happened, which is easy to work out from the above parameter...
    (mquery (list "__event" object :ondrop))))


;; when evaluating the action event is bound to the onload event from the file reader. By the time we have triggered this action the file has already been read.
;; if the file is large we might in theory want to display some progress indicator, but I'm not going to worry about that for now.
(defun file-drop (object action)
  ;; There's a slight lack of flexibility here. It might be better just to bind the event variable before evaluating the... just a mo...
  (event-handler object :ondrop (s "var r = new FileReader(); r.onload=function(event) {E=event;~A;};r.readAsDataURL(event.dataTransfer.files[0])"
                                   (if (stringp action)
                                       action
                                       (js:ps1* action)))))

;; NOTE - at present this only returns the file data - it ignores the mime type and file name. It would be useful to be able to (optionally perhaps) recover that information
(defun ajax-file-drop (object &key handler)
  (with-web-monad
    - (file-drop object (js.ajax-event object :file-drop :value 'event.target.result :handler handler))
    ;; then we return the value of whether or not the event happened, which is easy to work out from the above parameter...
    data <- (mquery (list "__event" object :file-drop))

    (if data
        ;; decode it...
        (unit (base64-decode (second (split "," data))))
        (unit nil))))



;; !!! These 2 seem never to have been used. REMOVE
#+nil(defun set-drag-data (object type data)
  (event-handler object :ondragstart `(event.data-transfer.set-data ,type ,data)))

#+nil(defun set-drag-file (object name url)
  (set-drag-data "DownloadURL" (s "application/octet-stream:~A:~A" name url)))

;; this is a very thin wrapper at the moment
(defun draggable (name monad &key (type :div) style class redraw)
  (draw-context name monad :type type :style (append style (list :cursor "pointer"))
                :class class :redraw redraw :draggable t))

;; NOTE - the image upload widget could probably be refactored a bit to use the file-drop function above. I imagine this would simplify thing a little, although it does a bunch of moderately clever stuff too.

;; an action to bookmark a page...
(defun bookmark-page (title url)
  `(cond (window.sidebar
          (window.sidebar.add-panel ,title ,url ""))
         (window.external
          (window.external.-add-favorite ,url ,title))))





;; This is a bit experimental at the moment, but it's working quite well...

;; Something like this:-
;; I guess row function could be a sexp representing JS code if we wanted and then it could use JS to generate each row
;; I would like to add something to trigger loading of all the rows optionally.
;; I also wonder if there could be a way to queue the requests so it does the ones we're looking at first rather than continuing to load other ones
;; that would make it tend to keep up with scrolling a lot better.
;; ANYWAY - for certain applications this will be very useful. Let's try it...
;; !!! Never used (generates compiler warnings)
#+nil(defun fast-delayed-table (name &key row-height height row-count row-function style redraw
                           ;; default to loading the next page as well. If we're paging down we should never notice the delay
                           (load-n-more (floor (+ 1 (/ height row-height)))))
  (flet ((draw-row (i)
           (draw-context (list name i)
                         (funcall row-function i)
                         :style (list :position "absolute"
                                      :top (px (* row-height i))))))
    (let ((displayed-rows (+ 1 (floor (/ height row-height)))))
      (with-web-monad
        ;; I could have some cunning JS figure out which rows are not visible and send that list to the update function
        ;; the update function could then, if desired, iterate over the list passed to it and draw those rows
        ;; therefore, the row drawing function should take a LIST of row indices. That will make things faster.
        ;; (only because of the overhead of connection start up)

        ;; now the cunning bit - draw some more rows...
        - (abel-ajax-context scroller-context
            (seq
              - (event-handler (list name "scroller") :onscroll
                               `((lambda ()
                                   (let ((html ""))
                                     (labels ((missing-rows (from to so-far)
                                                (if (< from to)
                                                    (if ($e (+ ,name "__" from))
                                                        (return (missing-rows (+ from 1) to so-far))
                                                        (progn
                                                          ;; mark the fact that we have requested it so we don't repeat the request before getting an answer...
                                                          (incf html
                                                                (+ "<div id='" ,name "__" from "' style='position:absolute; top: " (* ,row-height from) "px'><i style='color:#ddd'>loading...</i></div>"))
                                                          (return (missing-rows (+ from 1) to (+ so-far " " from)))))
                                                    (return so-far))))
                                       ;; figure out which rows should be visible...
                                       (let* ((scroller ($e ,(query-parameter-name (list name "scroller"))))
                                              (first-item (-math.floor (/ scroller.scroll-top ,row-height)))
                                              ;; I'm going to arbitrarily load 4 more items than will be displayed. This will tend to make it less obvious that we're demand loading. I could tweak this
                                              (last-item (+ first-item ,displayed-rows 1 ,load-n-more)))
                                         (if (> last-item ,row-count)
                                             (setf last-item ,row-count))
                                         (let ((missing (missing-rows first-item last-item ""))
                                               (content ($e ,(query-parameter-name (list name "content")))))
                                           (when missing
                                             ;; insert them onto the page (in one go)
                                             (incf content.inner-h-t-m-l html)
                                             
                                             ;; Ask the server for them
                                             ;; the only thing is I need to keep track of which ones I have requested. Wait a minute - I could draw dummies and then have the server do a replace...
                                             ;; (I would have to have JS draw the dummy which is marginally awkward but not too bad)
                                             ,(js.ajax-event (list name "scroller") :onscroll :value 'missing :handler scroller-context)))))))))
              


              value <- (mquery (list "__event" name "scroller" :onscroll))

              
              (if value
                  ;; All we have to do is replace the content of the empty cell now...
                  (mmapcar :web-monad
                           (lambda (index)
                             (redraw (list name index)
                                     (funcall row-function index)))
                           (mapcar #'parse-integer (all-matches-as-strings "\\d+" value)))
                  (unit nil))))

        (draw-context (list name "scroller")
                      (draw-context (list name "content")
                                    ;; to kick things off we will draw the first displayed-rows rows
                                    (mmapcar :web-monad
                                             #'draw-row
                                             ;; I was sure I had a function to do this:-
                                             (loop for i from 0 to (min (+ displayed-rows load-n-more)
                                                                        (- row-count 1))
                                                collect i))
                                    :style (list :height (px (* row-count row-height))
                                                 :overflow "hidden"))

                      :redraw redraw
                      :style (append (list :height (px (* row-height displayed-rows))
                                           :position "relative"
                                           :overflow "auto")
                                     style))))))


;; convenience things for use with the above

;; !!! NEVER USED
#|
(defmethod pseudo-cell ((monad web-monad) &key (width 100) (height 20) left (overflow "hidden"))
  (with-web-monad
    (melement monad
      (:span :style (css :width (px width)
                         ;; :overflow overflow
                         :position "absolute"
                         :top (px 0)
                         :left (when left (px left))
                         :display "inline-block")
             (draw it)))))

(defmethod pseudo-cell ((x t) &key (width 100) (height 20) left)
  (pseudo-cell (mhtml x) :width width :height height :left left))

(defun pseudo-row (height cells)
  (with-web-monad
    0 :== left-position
    
    (draw-context (js-next-window-id)
                  (mmapcar :web-monad
                           (lambda (cell)
                             (awhen (pseudo-cell (second cell)
                                                 :left left-position
                                                 :width (first cell)
                                                 :height height)
                               (incf left-position (first cell))
                               it))
                           cells)
                  :style (list :position "relative"))))

|#


;; Widgets using canvas...



;; now, should we want to make something scrollable we can place a scrollable thingy and it will
;; tell the canvas that it scrolled.
;; The canvas will draw on top of it and is expected to update itself as if it were the content
;; of the scrolling thing. Simples.


;; !!! I seem to be getting a bit stuck making a scrolling widget
;; I just want the canvas to stay still but to be able to use the scroll position to figure out
;; how to display the canvas - set its scroll offset property and re render
;; if I have the canvas on top of the scrolly thing I seem unable to scroll
;; underneath and the canvas doesn't get mouse events.

;; Could I just proxy some events? Maybe the mouse ones - just manually call the even handler
;; if there is one?
;; It's a thought.

(defun mwidget-scroll (content-name widget &key content-height content-width style)
  (declare (ignore content-width)) ; !!! why do we need this?
  (with-web-monad

    (query-parameter-name content-name) :== content-id
    
    - (event-handler (list content-name "scroller") :onscroll
                     `(let ((element ($e ,content-id))
                            (scroller ($e ,(query-parameter-name (list content-name "scroller")))))

                        ;; we should notify the content about scrollment by setting a property I guess
                        (element.set-property "scrollTop" scroller.scroll-top)))
    
    
    ;; these could be used to figure out scroll bar placement and so on
    ;; (width height) <- (dont-render widget)

    (event-handler (list content-name "content")
                   event `(let ((e ($e ,content-id)))
                            (,action event)))
    :== (proxy-event event action)

    - (proxy-event :onmousedown 'e.onmousedown)
    - (proxy-event :onmousemove 'e.onmousemove)
    - (proxy-event :onmouseup 'e.onmouseup)
    - (proxy-event :onclick 'e.onclick)
    
    
    (melement (mprogn widget
                      (draw-context (list content-name "scroller")
                                    (draw-context (list content-name "content")
                                                  (mhtml (:noescape "&nbsp;"))
                                                  :style (list :height (px content-height)))
                                    :style (list :position "absolute"
                                                 :overflow "auto"
                                                 :top 0 :bottom 0 :left 0 :right 0)))
      (:div :style (funcall #'css style)
            :id (:print (query-parameter-name (list content-name "container")))
       (:div :style (css :display "inline-block" :position "relative")
             (draw it))))))


;; next up: how to solve the inset scroll bar problem, how to link up the scroll events to set
;; properties

(defun js-block (code)
  (mhtml (:script (:noescape (:print (parenscript:ps1* code))))))

;; I don't think this should have a meaningful value or look at the request
;; it would have to be wrapped in order to do that
(defun mwidget (name render &key initial-state style class width height respond-to-mouse)
  ;; I'm not sure if the initial state is useful
  (declare (ignore initial-state))
  (with-web-monad
    - (draw-context name (unit nil) :type :canvas :style style :class class :width width :height height)

    - (if respond-to-mouse
          (mwidget-mouse-events name)
          (unit nil))
    
    - (js-block `((lambda (e)
                    (setf e.render ,render)
                    (setf e.properties (parenscript::create))
                    (setf e.set-property (lambda (name value dont-rerender)
                                           (setf (slot-value e.properties name) value)
                                           (unless dont-rerender
                                             (e.do-render))))
                    (setf e.do-render (lambda ()
                                        (let ((c (e.get-context "2d")))
                                          ;; pass in the context and the canvas element. Width and height aren't really required but we can pass them in for convenience. Likewise with properties
                                          (e.render c e e.width e.height e.properties))))
                    (setf e.previous-content-height 0)
                    (setf e.set-content-height (lambda (new-height)
                                                 (unless (= new-height e.previous-content-height)
                                                   (let ((c ($e ,(query-parameter-name (list name "content")))))
                                                     ;; when the canvas first renders itself the scroll content thingy won't be available unfortunately.
                                                     ;; maybe I should suppress the initial render and somehow require an explicit call to do the rendering. Not sure
                                                     (when c (setf c.style.height (+ new-height "px"))))
                                                   ;; by remembering the value we can make it safe to repeatedly set the content height in a tight loop with no performance issues. 
                                                   (setf e.previous-content-height new-height))))

                    (e.do-render))
                  ($e ,(query-parameter-name name))))
    
    
    ;; return the dimensions for use by a scroller...
    (unit (list width height))))


;; this function makes an mwidget update in response to low level mouse events
;; I don't suppose there's any use in this returning a value?
(defun mwidget-mouse-events (name)
  (with-web-monad
    - (event-handler name :onmousemove `(let ((e ($e ,(query-parameter-name name))))
                                          (let ((rect (e.get-bounding-client-rect)))
                                            (e.set-property "x" (- event.client-x rect.left) t) ; don't rerender yet...
                                            (e.set-property "y" (- event.client-y rect.top)))))

    - (event-handler name :onmousedown `(let ((e ($e ,(query-parameter-name name))))
                                          (e.set-property "mousedown" t)))

    - (event-handler name :onmouseup `(let ((e ($e ,(query-parameter-name name))))
                                          (e.set-property "mousedown" nil)))
    
    (unit nil)))

;; !!! I should try enter and leave too




;; Now, putting the above together we can make a lightning fast client side auto completing search thingamy

;; ??? What to do to close it?
;; Also, note that this does not enforce that the field has to be one of the chosen values at the moment. I should fix that.
(defun msearchable-drop-down (name values &key (height 200)
                                        (width 300)
                                        (item-height 30)
                                        (font-size 14)
                                        (font (s "~Apx Verdana" font-size))
                                        validate
                                        class
                                        default
                                        field-value)
  (with-web-monad
    - (draw-context (list name "bg") (unit nil)
                    :style (list ;; :background-color "rgba(0,0,0,0.3)"
                            :position "fixed"
                            :z-index "800"
                            :display "none"
                            :top 0 :left 0 :right 0 :bottom 0))
    value <- (minput (list name "field")
                    :formatter (lambda (value)
                                 (let ((value (strip-leading-and-trailing-space value)))
                                   (if (member value values :test #'equal)
                                       (list value value)
                                       (list value :error))))
                    :default default
                    :validate validate
                    :autocomplete "off"
                    :class class
                    :value field-value
                    )

    
    - (event-handler (list name "field") :onfocus `(progn
                                                     
                                                     (show ,(query-parameter-name (list name "options" "container")))
                                                     (show ,(query-parameter-name (list name "bg")))))
    ;; this prevents using the scroll bar:-
    ;; I think I need different criteria for hiding this thing. Not sure what though
    ;; definitely selection of an item
    ;; - (event-handler (list name "field") :onblur `(hide ,(query-parameter-name (list name "options" "container"))))

    - (event-handler (list name "field") :onkeyup `(let ((e ($e ,(query-parameter-name (list name "options")))))
                                                     (e.set-property "filter" this.value)))
    
    (js-next-window-id) :== data-id
    - (mhtml (:script "window." data-id " = " (output-js-array values)))
    
    - (js-block
       `(defun ,(intern (query-parameter-name  (list name "hide"))) (select)
          (let ((e ($e ,(query-parameter-name (list name "field"))))
                (field ($e ,(query-parameter-name (list name "options"))))) 
            (when (and field.properties.current-item select)
                    (setf e.value field.properties.current-item)
                    (let ((x ($e ,(query-parameter-name (list name "options")))))
                      ;; it makes sense to clear the filter when we are closed
                      (x.set-property "filter" "")))
            (e.blur)
            (hide ,(query-parameter-name (list name "bg")))
            (hide ,(query-parameter-name (list name "options" "container"))))))

    `(,(intern (query-parameter-name (list name "hide"))) ,select?) :== (hide select?)

    - (event-handler (list name "field") :onblur (hide t))
    - (event-handler (list name "bg") :onclick (hide nil))
    - (event-handler (list name "options") :onclick (hide t))
    
    - (mwidget-scroll (list name "options")
                      (mwidget (list name "options")
                               `(lambda (context canvas)

                                  ;; we can just set the 'data' property on a canvas and it will automatically redraw. I should just do this in a bit of script after this mwidget-scroll to do the initial load and render
                                  (unless canvas.properties.data (setf canvas.properties.data (slot-value window ,data-id)))

                                  (let ((data (if canvas.properties.filter
                                                  (let ((search (canvas.properties.filter.to-upper-case)))
                                                    (canvas.properties.data.filter (lambda (item)
                                                                                     (return (let ((item (item.to-upper-case)))
                                                                                               (> (item.index-of search) -1))))))
                                                  canvas.properties.data)))
                                    
                                    ;; this is safe to do each time through the loop...
                                    (canvas.set-content-height (* ,item-height data.length))
                                  
                                    ;; first clear the whole area...
                                    (setf context.fill-style (or canvas.properties.color "#f6f6f6"))
                                    (context.fill-rect 0 0 canvas.width canvas.height)
                        
                                    (setf context.fill-style "black")
                                    (setf context.font ,font)

                                    ;; then figure out which things we want to draw and where
                                    ;; ideally I might abstract this scrolling logic out a bit
                                    ;; I wonder how? If I provide a sort of table view like thing that would be one way
                                    (let ((scroll-top (or canvas.properties.scroll-top 0)))
                                      ;; now loop from the first to the last...
                                      (flet ((draw-from (first count)
                                               ;; figure out y coordinates for the given item...
                                               (when (< first data.length)
                                                 (let ((rect (parenscript:create :x 0
                                                                                 :y (- (* first ,item-height) scroll-top)
                                                                                 :width canvas.width
                                                                                 :height ,item-height)))

                                                   ;; now we need to highlight the one the mouse is over...
                                                   (when (and canvas.properties.x canvas.properties.y
                                                              (< canvas.properties.x (+ rect.x rect.width -30)) ; add a 30 pixel dead zone
                                                              (> canvas.properties.y rect.y)
                                                              (<= canvas.properties.y (+ rect.y rect.height)))
                                                     ;; make a note that it's the current item
                                                     (setf canvas.properties.current-item (elt data first))
                                                     (context.save)
                                                     (setf context.fill-style "#ddd")
                                                     (context.fill-rect rect.x rect.y rect.width rect.height)
                                                     (context.restore))
                                               
                                                   (context.fill-text (elt data first)
                                                                      20
                                                                      ;; for font height of 14 we do item-height - font-height / 2 
                                                                      (+ 8 14 rect.y)))
                                             
                                                 (when (> count 1) (draw-from (+ first 1) (- count 1))))))

                                        (draw-from (-math.floor (/ scroll-top ,item-height))
                                                   (+ (-math.ceil (/ ,height ,item-height)) 1))))))
               
                               :respond-to-mouse t
                               :width width :height height)

                      :style (list :display "none" :position "absolute" :z-index 900)
                      :content-height (* item-height (length values)) ; it must be given an initial height
                      :content-width 200)

    (unit value)))




;; ANIMATION !!!

;; -- can I start to compose animations somehow? I would imagine it should be possible. Maybe I could use the manimate-render to just animate the values of certain properties.
;;    the way this one is implemented it only animates by repeatedly calling render
;;    instead we could make an initialise and an update and have the render separate
;;    anyway, here's my first attempt:-


;; this makes the render of a widget be animated by requesting animation frames every (presumably) 60th of a second
(defun manimate-render (widget &key init step)
  (with-web-monad
    (js-block `((lambda ()
                  (let ((step nil)
                        (widget ($e ,(query-parameter-name widget)))
                        (previous-time 0))
                    ,@ (when init
                         `((,init widget widget.properties)))
                    (setf step (lambda (timestamp)
                                 ;; first frame will render with deltat = 1ms
                                 (when widget
                                   (widget.set-property "timestamp" timestamp t) ; don't re-render yet
                                   (widget.set-property "deltat" (if (= previous-time 0)
                                                                     0 ; 0 for deltat. This has special significance and must be handled properly if your animation uses deltat 
                                                                     (/ (- timestamp previous-time) 1000)) t)

                                   ;; now do the step if we have one...
                                   ,@(when step
                                           `((unless (= previous-time 0)
                                               (,step widget widget.properties))))
                                   
                                   (widget.do-render))
                                 
                                 (setf previous-time timestamp)
                                 (window.request-animation-frame step)))
                    (window.request-animation-frame step)))))))


#+nil(register-monadic-handler
      (with-web-monad
        - (abel-page :draw-header nil)
   
        - (mwidget "foo"
                   `(lambda (context canvas)
                      ;; !!! This suppresses the initial render and ONLY renders in an animation frame
                      (when canvas.properties.timestamp
                        (let ((p canvas.properties))
                          (unless p.count (setf p.count 0))
                          (unless (> p.count 10)
                            (console.log (+ "Delta t = " p.deltat))
                            (incf p.count))

                          (if (= p.deltat 0)
                              ;; do initialisation
                              (setf p.dx 50 ; pixels/second
                                    p.dy 50
                                    p.x (/ canvas.width 2)
                                    p.y (/ canvas.height 2))

                              ;; do next step
                              (progn
                                (incf p.x (* p.dx p.deltat))
                                (incf p.y (* p.dy p.deltat))

                                (when (or (and (> p.dx 0) (> p.x (- canvas.width 20)))
                                          (and (< p.dx 0) (< p.x 0)))
                                  (setf p.dx (- p.dx)))

                                (when (or (and (> p.dy 0) (> p.y (- canvas.height 20)))
                                          (and (< p.dy 0) (< p.y 0)))
                                  (setf p.dy (- p.dy)))))
                     
                     
                          (setf context.fill-style "black")
                          (context.fill-rect 0 0 canvas.width canvas.height)

                          (setf context.fill-style "red")
                          (context.fill-rect p.x p.y 20 20))))
                   :width 400 :height 300)

        ;; !!! This has to be after at the moment - fixme
        ;; (maybe I can use events to fix this problem)
        - (manimate-render "foo")
   
   
        (unit nil))
      :id "foo")




;; version 2...
#+nil(register-monadic-handler
      (with-web-monad
        - (abel-page :draw-header nil)
   
        - (mwidget "foo"
                   `(lambda (context canvas)
                      ;; !!! This suppresses the initial render and ONLY renders in an animation frame
                      (when canvas.properties.timestamp
                        (let ((p canvas.properties))
                     
                          (setf context.fill-style "black")
                          (context.fill-rect 0 0 canvas.width canvas.height)

                          (setf context.fill-style "red")
                          (context.fill-rect p.x p.y 20 20))))
                   :width 400 :height 300)

        ;; !!! This has to be after at the moment - fixme
        ;; (maybe I can use events to fix this problem)
        ;; so, this manimate-render could easily be made into a function:-
        - (mbounce-object 'p.dx 'p.dy 'p.x 'p.y )
        - (manimate-render "foo"
                           :init `(lambda (canvas p)
                                    (setf p.dx 50 ; pixels/second
                                          p.dy 50
                                          p.x (/ canvas.width 2)
                                          p.y (/ canvas.height 2)))

                           :step `(lambda (canvas p)
                               
                                    (incf p.x (* p.dx p.deltat))
                                    (incf p.y (* p.dy p.deltat))

                                    (when (or (and (> p.dx 0) (> p.x (- canvas.width 20)))
                                              (and (< p.dx 0) (< p.x 0)))
                                      (setf p.dx (- p.dx)))

                                    (when (or (and (> p.dy 0) (> p.y (- canvas.height 20)))
                                              (and (< p.dy 0) (< p.y 0)))
                                      (setf p.dy (- p.dy)))))
   
        (unit nil))
      :id "foo")

;; this needs to know where the edges are
;; I could also add in gravity and that sort of thing.
;; I could make a whole bunch of these functions and, maybe, compose these behaviours fairly easily???
;; Does it work to just use multiple manimate calls? Maybe. I'd have to probably suppress the request animation frame though - I wouldn't want to get multiple ones of those
(defun mbounce (x y dx dy)
  "call as (mbounce '(x 50) '(y 50) '(dx 20) '(dy 20))"
  (error "Not finished!")
  (manimate-render "foo"
                   :init `(lambda (canvas p)
                            (setf ,@dx ,@dy ,@x ,@y))
                   

                   :step `(lambda (canvas p)
                               
                            (incf p.x (* p.dx p.deltat))
                            (incf p.y (* p.dy p.deltat))

                            (when (or (and (> p.dx 0) (> p.x (- canvas.width 20)))
                                      (and (< p.dx 0) (< p.x 0)))
                              (setf p.dx (- p.dx)))

                            (when (or (and (> p.dy 0) (> p.y (- canvas.height 20)))
                                      (and (< p.dy 0) (< p.y 0)))
                              (setf p.dy (- p.dy))))))

;; NEXT UP: what will be a nice way to compose animations?



;; sudden realization: it would be easy to make a multi part web-monad form given a list of monads where the value of the form is a list of the values of each thing if valid
;; this would be a simple function
;; it would provide a much nicer way of dealing with complex form application sequences


;; How do I deal with logic for deciding when it's possible to 
;; !!! Not battle tested
(defun mmulti-pane-form (name items &key
                                      (next-button (lambda (n)
                                                     (declare (ignore n))
                                                     (msubmit (list name "next") "NEXT >")))
                                      (previous-button (lambda (n)
                                                         (declare (ignore n))
                                                         (msubmit (list name "previous") "< PREVIOUS"))))
  (with-web-monad

    current <- (mquery (list name "current"))

    (if current  (parse-integer current)  0)
    :== current

    max-progress <- (mquery (list name "max-progress"))

    (if max-progress (parse-integer max-progress) 0)
    :== max-progress
    
    next     <- (dont-render (funcall next-button current))
    previous <- (dont-render (funcall previous-button current))

    (when next (setf max-progress (max current max-progress)))
    
    (when next (incf current))
    (when previous (decf current))

    ;; bound it from 0 to length-1
    (setf current (min (max 0 current) (- (length items) 1)))
 
    
    ;; the expression below will have a chance to override the current pane if the monad we iterate over returns :error
    value <- (let ((overrode-current nil))
               (mmapcar-i :web-monad
                          (lambda (index item)
                            (seq
                              ;; value <- (dont-render item) ; using the below thing avoids repeatedly binding in the monad
                              (value output) <- (value-and-render item)
                              (when (and (not overrode-current)
                                         (eq value :error)
                                         ;; only jump backwards for errors
                                         (< index current))
                                (setf overrode-current t)
                                (setf current index))
                            
                              - (if (= index current)
                                    (draw-context (list name index)
                                                  (mshow output))
                                    (if (<= index max-progress)
                                        (draw-context (list name index)
                                                      (mshow output) :style (list :display "none"))
                                        (unit nil)))
                              (unit value)))
                          
                          items))

    (= current (- (length items) 1)) :== last-item
    (= current 0)                    :== first-item

    ;; now to get next and previous buttons to work we will need to update the current when one is pressed
    - (if first-item  (unit nil)     (funcall previous-button current))
    - (if last-item   (unit nil)     (funcall next-button current))
    
    ;; show the current tab for the moment...
    ;; - (minput (list name "current") :value (s "~A" current))
    - (mhtml (<> :input :type "hidden" :name (list name "current")
                 :value (s "~A" current)))
    
    
    - (mhtml (<> :input :type "hidden" :name (list name "max-progress")
                 :value (s "~A" max-progress)))

    (unit value)))



;; This is a stupidly simple HTML templating system for web monads
(defun mhtml-from-template (template &rest parameters)
  (loop for (name value) on parameters by #'cddr
     do (setf template
              (regex-replace-all (s "<%~A%>" (string-downcase (symbol-name name)))
                                 template
                                 value)))

  (let ((pieces (split "<%CONTENT%>" template)))
    (mhtml* (html (:noescape (:print (first pieces))))
            (yield)
            (html (:noescape (:print (or (second pieces) "")))))))




;; this is a general function for values which can be updated and are stored on the web page
;; stores a value on the web page which can be updated
(defun mupdatable (name update &key initial)
  (with-web-monad
    old <- (mquery name)
    (funcall update (or old initial)) :== new
    - (draw-context (list name "field")
                    (mhtml (:input :type "hidden" :name (:print (query-parameter-name name))
                                   :value new))
                    :type :span
                    :redraw (not (equal old new)))
    (unit new)))
 

(defun mradio-buttons (name values &key 
                                     class 
                                     (style (list :width "auto" :height "auto" :padding "0" :margin-top (px 7)))
                                     default)
  (with-web-monad
    value <- (mquery name)
    - (mmapcar :web-monad
               (lambda (x)
                 (let ((id (js-next-window-id)))
                   (mhtml (<> :input :id id :style style
                              :class class
                              :type "radio"
                              :name name :value x
                              :checked (when (equal x (or value default)) "checked")
                              (output-html-events))
                          (:label :for id " " x " "))))
               values)
    (unit value)))

 
;; This is a very simple hide/show button.
;; I could make it toggle its label
;; I could also have two buttons only one of which is displayed at a time and cross fade between them when they are pressed(!) That could look fancy.
(defun hide/show-button (target &key label)
  (with-ids (hs-button)
    (with-web-monad
      - (mbutton hs-button label)
      (event-handler hs-button :onclick (js-animate target :transition 'glow.anim.slide-toggle)))))


(defun in-div (monad &key class id)
  (melement monad
    (<> :div :class class
       :id id
       (dhtml (draw it)))))



(defun (setf js.disabled) (value object)
  `(setf (slot-value ($e ,object) 'disabled) ,(or (and value t)
                                                  'js::false)))

