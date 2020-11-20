
(in-package :web-monad)



;; NOTES

;; The question occurs to me of whether it's really worth having the
;; new-page-p protocol. It seems that any time we want to nest
;; anything with mhtml-let or anything like that we have to explicitly
;; pull out new pages anyway because there's no sensible way of
;; handling that. It would be nice if I didn't have to do that, but it
;; seems I do.

;; BUT maybe I'm doing something wrong, because mweb-page (for
;; example) nests it's content as well, but we can still create a new
;; page in the middle of an existing page and it Just Works (tm). I
;; think I'm missing something here...

;; Maybe the thing with mhtml-let is that it sort of has multiple
;; yield points. Just doing an explicit yield is fine, but when you
;; have multiple things to draw it gets a bit interesting.

;; I have made a solution for this. Hurrah.


;; NEW PROBLEM

;; !!! I need to put the monadic stuff and the bundle code into the load sequence for ABEL. I should test it on the test server (because I need to test the whole process including approval)



;; This should do this globally for this file I think
(declaim (optimize (speed 2) (debug 1)))


(defstruct web-monad handler)

(defstruct web-thunk value render new-page-p event-handlers ajax-render stop)

;; Stuff for registering things to respond to HTTP requests...

(defparameter *monad-dispatch-table* (make-hash-table :test #'equal))
(defparameter *m-entity* nil)

(defparameter *monadic-handler-check-functions* (make-hash-table :test #'equal))






(defmacro output-html-events ()
  `(output-events-for-object name +event-handlers (first +other-parameters)))


;; This ends request handling immediately - no more of the monad will happen
(defun end-request (&optional value)
  (make-web-monad :handler
                  (lambda (request)
                    (declare (ignore request))
                    (make-web-thunk :value value
                                    :stop t))))



;; I'm going to slightly extend the macrology here in ways that make it easier to diagnose problems. This should be done for all monads, but I'll put off reimplementing it all for the time being...

;; unless we have reason to think otherwise the answer is no
(defmethod is-monadic-value ((monad t) value) 
  (declare (ignore value))
  nil)

;; things like this will need to be defined:-
(defmethod is-monadic-value ((monad (eql :web-monad)) (value web-monad)) t)



(defun check-monadic-type (expression monad value)
  ;; I can use the method defined before but just handle the error I suppose?
  ;; is-monadic-value will have to be defined for each type of monad
  (if (is-monadic-value monad value)
      value
      (error "Expression ~S did not return monad of type ~A but returned ~S instead."
             expression monad value)))

;; (check-monadic-type 'a :web-monad (+ 1 2))

;; !!! Should this be moved out of here?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-bind-form (m k monad)
    ;; we have to have a look at the structure of m...
    ;; skip the name mangling path, although I might look into it in the future for compile time checking. It breaks compatibility though...
    #+nil(if (and (consp m)
                  (symbolp (car m)))
             `(typed-bind ,(cons (intern (s "~A//~A" (car m) monad)
                                         (symbol-package (car m)))
                                 (cdr m))
                          ,k)
             `(typed-bind (check-monadic-type (quote ,m) ,monad ,m)
                          ,k))
    ;; This just inserts a type check in bind's first argument 
    ;; I could check the result of calling k too by wrapping the whole thing, although the info that gives us is less useful so I won't bother.
    ;; I suppose I'm really slowing down function calling here and I should use some more efficient macrology, but I'll leave as is for now.
    `(typed-bind (check-monadic-type (quote ,m) ,monad ,m)
                 ,k)))


;; (transform-bind-form 'a 'k :web-monad)
;; (transform-bind-form '(f x) 'k :web-monad)

(defun typed-bind (m k) (bind m k))

(defmacro with-new-bind (monad &body forms)
  `(macrolet ((bind (m k)
                (transform-bind-form m k ,monad)))
     (with-monad ,monad
       ,@forms)))





(defmacro with-web-monad (&body body)
  #+nil`(with-monad :web-monad
          ,@body)
  `(with-new-bind :web-monad
     ,@body))


(defmethod monad-return ((monad (eql :web-monad)) value)
  (make-web-monad :handler
                  #'(lambda (request)
                      (declare (ignore request))
                      ;; just return the value - no code for rendering
                      (make-web-thunk :value value))))

(defmethod check-monad-return-type ((monad (eql :web-monad)) (value web-monad)) value)

;; because I might have to do this elsewhere too I'm going to factor out the binding together of two web thunks to product a new one...
(defun bind-web-thunks (result continue-result)
  (let ((renderer (web-thunk-render result))
        (new-page-p (web-thunk-new-page-p result))
        (ajax-renderer (web-thunk-ajax-render result))
        
        ;; now what do I do?
        (continue-on-new-page-p (web-thunk-new-page-p continue-result))
        (continue-renderer (web-thunk-render continue-result))
        (continue-ajax-renderer (web-thunk-ajax-render continue-result)))

    (flet ((create-renderer-pipeline (renderer continue-renderer)
             (if (or continue-on-new-page-p (not renderer))
                 continue-renderer
                 ;; Some things won't need to render anything so this slot will be empty
                 #'(lambda (k handlers &optional ajax-context)
                     (declare (ignore ajax-context))
                     (funcall renderer
                              (if continue-renderer
                                  #'(lambda (handlers)
                                      (funcall continue-renderer k handlers))
                                  k)
                              handlers)))))
      (make-web-thunk :value (web-thunk-value continue-result)
                      ;; If the continue wants a new page then so do we...
                      :new-page-p (or new-page-p continue-on-new-page-p)
                                        
                      ;; log all the event handlers...
                      :event-handlers (if continue-on-new-page-p
                                          ;; if we aren't going to render anything from the past then we certainly won't respond to past events.
                                          (web-thunk-event-handlers continue-result)

                                          (append (web-thunk-event-handlers result)
                                                  (web-thunk-event-handlers continue-result)))

                      :ajax-render (create-renderer-pipeline ajax-renderer continue-ajax-renderer)
                      :render (create-renderer-pipeline renderer continue-renderer)
                      :stop (web-thunk-stop continue-result)))))



(defmethod bind ((m web-monad) f)
  (make-web-monad :handler
                  #'(lambda (request)
                      (let ((result (web-monad-handle-request m request)))
                        (if (web-thunk-stop result)
                            result
                            (let* ((continue (funcall f (web-thunk-value result)))
                                   (continue-result (web-monad-handle-request continue request)))
                              (bind-web-thunks result continue-result)))))))



;; if we don't know whether a parameter is a web monad or not (so as to allow both for convenience) I can use this...
(defmethod as-web-monad ((a t))
  (with-web-monad (unit a)))

(defmethod as-web-monad ((a web-monad))
  a)

;; now a macro for rendering html...
;; it has to sort of compose like bind, although not quite the same...


;; fundamental components needed to make it all go...


;; This isn't actually monad specific
(defun testable-query-value (name request)
  (let ((qname (query-parameter-name name)))
    (cond ((consp request)
           (second (find qname request :key #'first :test #'equal)))
          ((not request) nil)
          ((and (stringp name) 
                (scan "\\[\\]$" name))
           (loop for entry in (net.aserve::request-query request)
              when (string= (car entry) name)
              collect (cdr entry)))
          (t (request-query-value qname request)))))
                                                     

(defun mquery (name &key default)
  (with-web-monad
    (make-web-monad :handler
                    #'(lambda (request)
                        (make-web-thunk :value (or (testable-query-value name request)
                                                   default))))))

(defun mheader-slot (name)
  (make-web-monad :handler
                  #'(lambda (request)
                      (make-web-thunk :value (net.aserve:header-slot-value request
                                                                           name)))))
;; (mheader-slot :referer)


(defun muser-account ()
  (mquery "__USER"))

;; in order to defer rendering and still have everything bound we can use this function to get access to the thunk itself. This returns a new thunk in the monad though, to make sure everything is threaded through.
;; !!! If I add anything else to the monad result I have to add it here too.
;; This is used to embed monadic things in mhtml forms...
;; NOTE - this propagates event handlers to the containing thunk, which is good
(defun monad-thunk (monad)
  (make-web-monad :handler #'(lambda (request)
                               (let ((result (web-monad-handle-request monad request)))
                                 (make-web-thunk :value result
                                                 :new-page-p (web-thunk-new-page-p result)
                                                 ;; Even though we don't want to do normal rendering at this point, if the bound thingy performs some AJAX action we do want to do that, ergo:-
                                                 :ajax-render (web-thunk-ajax-render result)
                                                 :event-handlers (web-thunk-event-handlers result))))))

;; this macro is used to render a defered thunk...
;; it relies on the name of the parameter in the macro, which isn't great - I should macro let and gensym, but this makes things easier to debug.
;; I might change it afterwards once it's fully working
(defmacro draw-thunk (thunk)
  `(apply #'render-web-thunk
          (cons ,thunk
                (cons +event-handlers
                      +other-parameters))))

;; I don't quite see why we need to complicate the above with so many things. I think we can just use this:-
;; This just puts stuff into the rendering pipeline...
;; !!! couldn't I just define this in terms of the next one trivially?
(defparameter *monadic-event-handlers* nil)

(defmacro mhtml (&body forms)
  "web-monad wrapper around gigamonkeys html macro: (with-web-monad - (mhtml (:h1 \"Some HTML\"))) "
  `(make-web-monad :handler
                   #'(lambda (+request)
                       (declare (ignorable +request))
                       (make-web-thunk :render #'(lambda (k +event-handlers &rest +other-parameters)
                                                   (let ((*monadic-event-handlers* +event-handlers))
                                                     (html ,@forms))
                                                   (apply k (cons +event-handlers +other-parameters)))))))

;; This one doesn't yield automatically after displaying all its output...
;; !!! NOTE - for reasons of being usable for returning the whole response this does NOT put in an (html) form
(defmacro mhtml* (&body forms)
  `(make-web-monad :handler
                   #'(lambda (+request)
                       (declare (ignorable +request))
                       (make-web-thunk :render #'(lambda (k +event-handlers &rest +other-parameters)
                                                   (declare (ignorable k +event-handlers +other-parameters))
                                                   (flet ((yield ()
                                                            (apply k (cons +event-handlers +other-parameters))))
                                                     ,@forms))))))


;; as a convenience this simplifies grabbing the thunk and getting its renderer and value. It's pretty straightforward
(defmacro mhtml-let (bindings &body forms)
  `(macrolet ((value (x)
                `(web-thunk-value ,x))
              (draw (x)
                `(draw-thunk ,x))
              (mvalue (x)
                `(if (web-thunk-new-page-p ,x)
                     (make-web-monad :handler #'(lambda (request)
                                                  (declare (ignore request))
                                                  ,x)) ; if this asked for a new page then we should just return this verbatim...
                     (unit (web-thunk-value ,x)))))
     (seq ,@(reduce #'append
                    (loop for (name value) in bindings
                       collect `(,name <- (monad-thunk ,value))))
        
          ,@forms)))

;; convenience for a common case where we just want to wrap something in an element.
;; In fact, it will probably be pretty rare that we need anything more than this.
(defmacro melement (monad &body element)
  `(with-web-monad
     (mhtml-let ((it ,monad))
       - (mhtml ,@element)
       (mvalue it))))


;; This suppresses the rendering of any monadic value passed to it, but leaves everything else unchanged
(defun dont-render (monad)
  (make-web-monad :handler
                  #'(lambda (request)
                      (let ((thunk (web-monad-handle-request monad request)))
                        (make-web-thunk :value (web-thunk-value thunk)
                                        :event-handlers (web-thunk-event-handlers thunk))))))


;; !!! NEW - mechanism for evaluating a monad just once and being able to (conditionally) render it
(defun value-and-render (monad)
  (make-web-monad :handler
                  #'(lambda (request)
                      (let ((thunk (web-monad-handle-request monad request)))
                        (make-web-thunk :value (list (web-thunk-value thunk)
                                                     thunk)
                                        :event-handlers (web-thunk-event-handlers thunk))))))

;; render will actually just be a thunk
(defun mshow (render)
  (make-web-monad :handler
                  #'(lambda (request)
                      (declare (ignore request))
                      render)))


(defun mhidden-field (name &key default)
  (with-web-monad
    value <- (mquery name :default default)
    - (mhtml (<> :input :name (query-parameter-name name)
                 :type "hidden"
                 :value (or value "")
                 (output-html-events)))
    (unit value)))


;; given a list of monads this returns the first value of them which is true if any, otherwise the true value which was posted before
;; This is useful to make buttons for selecting things and then remembering which of the things was selected

;; this has a limitation in only being able to return string values properly, so make sure the monads
;; return strings 
;; !!! Couldn't we generalise this? It's similar to what the multi panel form uses.
;;     Is it worth having a simple monadic widget with an 'override' thing
(defun mone-of (name items)
  (with-web-monad
    values <- (mmapcar :web-monad
                       (@@ ?1)
                       items)
    (first (filter (@@ ?1) values)) :== value
    (if value
        (seq - (mhtml (<> :input :name (query-parameter-name name)
                          :type "hidden"
                          :value value))
             (unit value))
        (mhidden-field name))))



;; this returns a new response...
(defun mnew-reply ()
  (make-web-monad :handler #'(lambda (request)
                               (declare (ignore request))
                               (make-web-thunk :new-page-p t))))




;; !!! What about error reporting for AJAX calls?
(defun mhttp-response (&key http-header (content-type "text/html") (response *response-ok*) (timeout 120) session-name)
  (declare (ignore session-name))
  (with-web-monad
    - (mnew-reply)
    ;; basically do the same thing for AJAX too...
    - (ajax-render (mhtml*
                     (if (and +request (not (consp +request)))
                         (with-http-response (+request *m-entity* :content-type "text/plain" :response response :timeout timeout)
                           (when http-header (funcall http-header +request))
                           (with-http-body (+request *m-entity*)
                             (with-html-output ((request-reply-stream +request))
                               (html (:l (yield))))))
                         (html (:l (yield))))))
    (mhtml*
      (if (and +request (not (consp +request)))
          (with-http-response (+request *m-entity* :content-type content-type :response response :timeout timeout)
            (when http-header (funcall http-header +request))
            ;; by default there won't be a session because this will default to nil BUT you can use the session if you want...
            (with-http-body (+request *m-entity*)
              (with-html-output ((request-reply-stream +request))
                (yield))))
          (yield)))))


(defun mrequest ()
  (make-web-monad :handler #'(lambda (+request)
                               (make-web-thunk :value (and +request
                                                           (not (consp +request))
                                                           +request)))))


;; !!! This is a GETTER ONLY for session variables. It does not establish a session if none exists so we can't set up session things from a web monad without writing some more stuff. I'm just writing this for a very specific purpose...
(defun msession-datum (key &key (name "<default>"))
  (with-web-monad
    req <- (mrequest)
    (session:get-request-session name req) :== sess
    (unit (when sess
            (gethash key (cdr sess))))))


;; This is the SETTER. Still, it does not establish a session if none exists...
(defun set-msession-datum (key value &key (name "<default>"))
  (with-web-monad
    req <- (mrequest)
    (session:get-request-session name req) :== sess
    (unit (when sess
            (setf (gethash key (cdr sess)) value)))))

(defsetf msession-datum set-msession-datum)


(defun monadic-post-handler (request)
  "Don't append a /p at the end of the uri. Also don't write Shakespeare"
  (let ((uri (request-uri request)))
    (format nil "~A~A"
            (puri:uri-path uri)
            (if (puri:uri-query uri)
                (format nil "?~A" (puri:uri-query uri))
                ""))))

(defun mmonadic-post-handler ()
  (make-web-monad :handler #'(lambda (request)
                               (make-web-thunk :value (monadic-post-handler request)))))

;; !!! This should be much improved. We should show the backtrace for one thing
(defun report-error (err)
  ;; Why do I need to do this here?
  ;; (dquery "ROLLBACK WORK")
  (html (:h1 "Rendering Error")
        (:p  err)
        ;; open a comment to suppress the inevitable error - we must throw again so as to abort the database transaction
        ))

;; simple web page...
(defun mweb-page (&key header (timeout 120) (post-handler nil) (body-style "") session-name)
  (with-web-monad
    - (mhttp-response :timeout timeout :session-name session-name)
    (mhtml*
      (html (:noescape "<!DOCTYPE html>")
            (:html
              ;; !!! NOTE the header is not bound at the moment - we just run it separately
              ;; I could bind it by sticking it above in (dont-render). I'm not going to worry for the time being though
              (:head (when header
                       (web-monad-run header +request))
                     
                     ;; !!! Shall I remove this as a dependency? It's a simple browser test
                     (:script :type "text/javascript" :src "/pub/scripts/ie.js")
                     (:script :type "text/javascript" :src "/pub/scripts/displayServer.js")
                     (:script :type "text/javascript" :src "/pub/scripts/glow/1.6.0/core/core.js")
                     (:script :type "text/javascript" :src "/pub/scripts/scriptaculous/lib/prototype.js")
                     (:script :type "text/javascript" :src "/pub/scripts/scriptaculous/src/scriptaculous.js"))
              ;; !!! In order to make these pages do anything interesting we need to have this form element.
              (:body :style body-style
                     (:form :method "POST"
                            :action (:print (or post-handler (monadic-post-handler +request)))
                            ;; Surely there should be a better way of doing this than having the handler-case as well? I just don't want the error to proceed downwards and be caught by the dispatch handler as well
                            ;; since that will try and generate a complete HTTP response.
                            ;; Now as long as we don't error before outputting the response header (above) we should be fairly safe from invisible errors
                            (handler-case
                                (handler-bind ((t #'(lambda (c)
                                                      (report-error c))))
                                  (yield))
                              (t (c)
                                (declare (ignore c))
                                nil)))))))))



;; this is a low level call. Normally you'll want to redirect to a monad
(defun mhttp-redirect (target)
  (mhttp-response :response *response-found*
                  :http-header #'(lambda (request)
                                   (setf (reply-header-slot-value request :location)
                                         target))))


;; Does this need to know the user?
;; Do I really need user information for anything?
;; maybe relative t should be the default? It shouldn't generally cause problems I don't think (famous last words)
(defun monad-redirect (monad &key id one-shot user prefix relative (url-prefix ""))
  (with-web-monad
    ;; propagate the user account from the current request by default
    user-account <- (muser-account)
    ;; It might seem strange to give an explicit id here, but it can be useful if we want to avoid forking history type situations (see my-excess-protected for the reasoning for this)
    (awhen (register-monadic-handler monad :id id :prefix prefix :one-shot one-shot :user (or user user-account))
      (if relative
          (mhttp-redirect (concatenate 'string url-prefix (regex-replace ".*/" it "")))
          (mhttp-redirect (concatenate 'string url-prefix it))))))











;; how to do links? Linking to a monad would be the obvious thing to do.
;; We can then generate an ID for the monad. The monad must return an HTTP response.
;; We can make one shot links by linking to something that redirects twice.

;; now that we have the foundation we can define things that use this monad...


;; the value of this will be
;; it might be good to make this more flexible in terms of style and things like that. Maybe I can pass a monad
(defun mlink-to (target text)
  (mhtml
    (:a :href target text)))

;; This is probably more useful. It will OUTPUT a URL to a monad
;; the other parameters are options to the register monadic handler function
(defun url (monad &rest options)
  (html (:print (apply #'register-monadic-handler (cons monad options)))))

;; the problem with linking like that is that we won't have the values of the bindings which are retrieved from posted parameters. Most of the time I don't think I would want to use that.
;; There are certain instances, though, where it could be useful.
;; in this case we could perform some action and then redirect to display something. Like this...

(defun action-redirect (action result)
  (monad-redirect (with-web-monad
                    - action
                    (monad-redirect result))
                  :one-shot t))









(defun monad-dispatch-id (&optional prefix)
  (let ((id (random-string 20)))
    (awhen (if (gethash id *monad-dispatch-table*)
               ;; try again - though the chance is vanishingly small
               (monad-dispatch-id)
               id)
      (if prefix
          (concatenate 'string prefix it)
          it))))

;; (monad-dispatch-id)

;; I probably need to create some kind of structure for monad registration...
(defstruct monad-url-handler
  monad user name last-access-time one-shot lifespan)

(defun unregister-monad (id)
  (awhen (gethash id *monad-dispatch-table*)
    (remhash id *monad-dispatch-table*)
    it))

;; I hope this doesn't get too slow. Maybe I should put a lock around it?
(defun expire-old-monads (&key active-user)
  (let ((keys (hash-keys *monad-dispatch-table*)))
    (loop for key in keys
       for handler = (gethash key *monad-dispatch-table*)
       when (and active-user
                 (equal active-user (monad-url-handler-user handler)))
       do (setf (monad-url-handler-last-access-time handler)
                (get-universal-time))

       when (and (monad-url-handler-lifespan handler)
                 (> (- (get-universal-time) (monad-url-handler-last-access-time handler))
                    (monad-url-handler-lifespan handler)))
       do (unregister-monad key))))



(defun register-monadic-handler (handler &key prefix id user name one-shot (lifespan 3600) imortal validate)
  (let ((x (or id (monad-dispatch-id prefix))))
    (when validate
      (setf (gethash x *monadic-handler-check-functions*)    validate))
    (setf (gethash x *monad-dispatch-table*)
          (make-monad-url-handler :monad handler :user user :name name :one-shot one-shot
                                  :lifespan (unless imortal lifespan)
                                  :last-access-time (get-universal-time)))
    (concatenate 'string "/m/" x)))


;; I could make this into an ABEL page, but I haven't at the moment.
(defun error-page (err &optional (extra (with-web-monad (unit nil))))
  (with-web-monad
    - (mweb-page)
    ;; This shows an error message in AJAX mode if we don't even manage to output anything
    - (alert (s "Error: ~A"
                ;; (trivial-backtrace:print-backtrace err :output nil)
                err))
    - extra
    (mhtml
      (:h1 "Error")
      (:p err)
      (:pre #+nil(:print (trivial-backtrace:print-backtrace err :output nil))))))


(register-monadic-handler
 (with-monad :web-monad
   - (mhttp-response :response *response-not-found*)
   (mhtml (:html
            
            (:body
             (:h1 "Page Not Found")

             (:p "This could mean you need to log in again, or you may have ended up here by trying to repeat an action. You could try going back to the previous page you came from. ")))))
 
 :id "page-not-found"
 :imortal t)




;; if an error occurs here we will have to respond with an HTTP error response...
;; !!! FIXME - this needs to run an error monad instead. That monad will just reply with a 
(defun web-monad-handle-request (monad request)
  (funcall (web-monad-handler monad)
           request))

(defun render-web-thunk (thunk event-handlers &key (renderer #'web-thunk-render))
  ;; then invoke the renderer if there is one. There really should be otherwise this isn't going to respond to a request.
  (awhen (funcall renderer thunk)
    (funcall it #'(lambda (a)
                    (declare (ignore a)))
             event-handlers)))

;; I might have to have an error handler for web-monad-handle-request 
;; !!! FIXME - or we have to trap errors here. We can't do it outside of here though, because we wouldn't know if we've started to generate a response or not.
(defun web-monad-run (monad &optional request)
  (let ((result (web-monad-handle-request monad request)))
    
    (render-web-thunk result (web-thunk-event-handlers result)
                      :renderer (if (testable-query-value "__AJAX" request)
                                    #'web-thunk-ajax-render
                                    #'web-thunk-render))
    
    (web-thunk-value result)))




;; Making permanent named links...

;; There will need to be some way of getting a user identity in order
;; to use this. It doesn't matter what - presumably a login process
;; would identify a user and then create a named home page for that
;; user which linked to all the pages they were to have access
;; to. This does not say anything about what the user account
;; information should look like...

(defparameter *named-capabilities* (make-hash-table :test #'equal))

;; !!! There is one slight problem with these - they will never expire. There won't be a huge number of them, and we won't keep creating new ones all the time, but still.
;; also, they will point to non existent things after the target monad expires.

;; We just store a map from (name user) -> id
;; we always replace the existing definition with the given monad to make sure that any changed to the definition are accounted for...
(defun named-capability (monad user name)
  (if (gethash (list name user) *named-capabilities*)
      (register-monadic-handler monad :user user :id (regex-replace "\\/m\\/" (gethash (list name user) *named-capabilities*) "")) 
      (setf (gethash (list name user) *named-capabilities*)
            (register-monadic-handler monad :user user))))

;; NOTE - the above hash will be useful for looking at other people pages as if logged in as them.



;; JAVASCRIPT EVENT HANDLING...


(defstruct event-handler
  object event action context)

;; here is a low level function for putting in event handlers...

(defun event-handler (object event action)
  (make-web-monad :handler #'(lambda (request)
                               (declare (ignore request))
                               (make-web-thunk :event-handlers (list
                                                                (make-event-handler :object object :event event :action action))))))

(defmethod comparison-key ((x t))
  x)

(defmethod comparison-key ((x event-handler))
  (list (comparison-key (event-handler-object x))
        (event-handler-event x)
        (event-handler-action x)
        (event-handler-context x)))

;; the interesting bit comes when we try to interpret those events...
(defun events-for-object (object events)
  (let ((seen (make-hash-table :test #'equal))
        (object-name (unless (functionp object)
                       (query-parameter-name object))))
    (loop for e in (sort (filter (if (functionp object)
                                     object
                                     (lambda (x)
                                       ;; if it is a presentation type then in some cases a name cannot be generated for it (and it's a waste of time)
                                       (and (not (and (listp (event-handler-object x))
                                                      (eq :presentation (first (event-handler-object x)))))
                                            (equal object-name
                                                   (query-parameter-name (event-handler-object x))))))
                                 events)
                         #'string-lessp
                         :key (compose event-handler-event symbol-name))
         unless (gethash (comparison-key e) seen)
         collect e
         do (setf (gethash (comparison-key e) seen) t))))


;; then look for particular events...
;; this will be useful later for adding pseudo event handlers...
(defun events-of-type (type events)
  (filter (@ eq type (event-handler-event ?1))
          events))

;; then we can return all event handlers for a particular object. We should probably just sort all the events for the object by event type and then group them to output the events list.
;; the problem is that some of the events (like :onchange) won't be that simple to implement for all elements. I want them to work with input fields etc and that will involve writing timers and such.
;; This isn't going to be very complicated and will be easy to add in. Cool.

;; Then I will build on top of this to implement the AJAX calls too. That will need a little bit of thought.

;; I'm going to try memoizing this because it seems to be horrendously slow. This should help quite a lot
;; I think we can happily use equals on any js code, which is what the memoization table does. Hope so
(defun output-event-list (events &optional context)
  (let ((sorted-events (merge-sort events #'string-lessp 
                                   :key (compose event-handler-event symbol-name))))
    (loop for group in (group #'equal
                              sorted-events
                              :key (compose event-handler-event symbol-name))
       collect (event-handler-event (first group))
       ;; now I have to generate JS code for the whole group...
       collect (let ((events (loop for item in group
                                for action = (aif (event-handler-action item)
                                                  (if (functionp it)
                                                      (funcall it (or (event-handler-context item)
                                                                      context))
                                                      it))
                                when (stringp action)
                                collect action
                                when (consp action)
                                collect (js:ps* action))))
                 
                 (cond ((member (event-handler-event (first group)) '(:draggable :href))
                        (first events)) ; only ever consider the first href
                       ((eq :class (event-handler-event (first group)))
                        (string-list events " "))
                       (t (string-list events ";")))))))


;; this will need augmenting somewhat to handle a few other things...
;; this will need to exclude certain events...
(defun output-events-for-object (object events &optional context)
  (output-event-list (events-for-object object events) context))


;; to do an AJAX action we use this:-

;; then all I need to do is this:-
;; (just switch the render to an ajax render)
(defun ajax-render (monad)
  (make-web-monad :handler #'(lambda (request)
                               (let ((r (copy-web-thunk (web-monad-handle-request monad request))))
                                 ;; I don't really like this setf. I've made my own copy just in case this is ever an issue
                                 (setf (web-thunk-ajax-render r)
                                       (web-thunk-render r))
                                 (setf (web-thunk-render r) nil)
                                 r))))

;; I've split this out in case I want to trigger an AJAX event after an animated transition or in some other JS context.
;; it could be useful to trigger further events whilst handling an rcall too.
(defun js.ajax-event (object event &key (value 1) handler)
  (if handler
      `(ev ,(query-parameter-name (list "__event" object event))
           ,value ,handler)
      `(ev ,(query-parameter-name (list "__event" object event))
           ,value)))


;; Now I can use the above event-handler function to make a monad to respond to AJAX events...
(defun ajax-event (object event &key (value 1) handler)
  (with-web-monad
    - (event-handler object event (js.ajax-event object event :value value :handler handler))
    ;; then we return the value of whether or not the event happened, which is easy to work out from the above parameter...
    (mquery (list "__event" object event))))

;; This will work as a client side handler OR as a server side handler. Cunning eh?
;; although the way in which it is called must be different
(defun alert (value &key client)
  (with-web-monad
    - (if client
          (unit nil)
          (ajax-render (mhtml (:print (js:ps* `(alert ,value))))))
    (unit `(alert ,value))))

(defun redraw (id monad)
  (ajax-render
   (melement monad
     (:rp :id (:print (query-parameter-name id))
          (draw it)))))

(defun ajax-append (id monad)
  (ajax-render
   (melement monad
     (:ap :id (:print (query-parameter-name id))
          (draw it)))))

;; Load a new object...
(defun ajax-load (id monad)
  (ajax-render
   (melement monad
     (:ap :id (:print (query-parameter-name id))
          (draw it)))))



;; PROGRESS


;; Jobs a good un. I now have form POST foundations AND AJAX foundations (including client side JS stuff) in place.
;; I need to define more functions to use these, but this should now be fairly straightforward. It's looking good (very powerful)

;; There are a couple of extensions I need to make to this system: I should extend it so that I can respond only to certain key presses (eg, if escape is pressed)
;; I also need to respond to change events, which is slightly non trivial in the case of input fields, but shouldn't be hard to implement (using timers)
;; Also I need to pass parameters for some events
;; I would also like to find a simple way to record the scroll offset when it comes to popping up windows. In fact, how am I going to pop up windows? That's another thing I need to do.

;; finally, why isn't it working outside of the abel page? Did I miss out an important script? 

;; I can now (very) easily make things to auto submit the form in response to certain actions. Responding to user interaction should be a doddle. Now I just need to do Irish bundling and broker monthly statements!

;; I wonder if I can so rig things that if a minput has an attached formatter (which it probably always does) that any AJAX event triggers it to redraw? I think I can...


;; !!! REALIZATION
;; I've long been wondering how to deal with IDs since it didn't seem possible to generate them using (with-ids) because there's nowhere to store them across invocations of AJAX monads etc
;; BUT --- if I do a monad redirect (for example) I can bind the IDs to the closure which generates the page
;; This sort of thing has interesting consequences - some useful, some not so useful. On the whole, definitely useful though. 

;; !!! I Would like some way to easily position a box or window in the center of the window. If necessary I will use JS to get the scroll position of the window, but there should be a better way. It's annoying that IE doesn't support fixed position.



;; VARIOUS UTILITY THINGS

;; This is almost exactly the same as the pre monad js-animate function. It's important to be able to deal with compount IDs though
(defun js.animate (name &key (transition 'glow.anim.fade-out)
                   on-complete
                   ;; (duration (js-animate-duration 0.2))
                   (duration 0.2))
  
  (let ((id (query-parameter-name name)))
    (if (numberp duration)
        ;; just output the simple one. I seem to be having some trouble getting the slow mo key to work with this...
        `(,transition ,(s "#~A" id) ,duration ,@(when on-complete
                                                      `(:on-complete (lambda ()
                                                                       ,on-complete))))
        `((lambda (event)
            (,transition ,(s "#~A" id) ,duration ,@(when on-complete
                                                         `(:on-complete (lambda ()
                                                                          ,on-complete)))))
          event))))

;; I need to work out how to nicely organise all these bits of JS triggering code including a consistent way of dealing with client/server triggering...

;; this is a useful thing to do...
(defun js.set-slot-value (element property value)
  `(let ((object ($e ,(query-parameter-name element))))
     (when object
       (setf (slot-value object ',property)
             ,(if (eq value :toggle)
                  `(not (slot-value object ',property))
                  value)))))

;; (js.set-slot-value (list 1 2) 'checked t)
;; (js.set-slot-value (list 1 2) 'checked :toggle)

;; simple thing to remove an element...
(defun js.remove-element (name)
  (let ((id (query-parameter-name name)))
    `(let ((e ($e ,id)))
       (when e
         (e.parent-node.remove-child e)))))

(defun js.fade-out-element (name)
  (js.animate name :transition 'glow.anim.fade-out
              :duration 0.2
              :on-complete (js.remove-element name)))

(defun js.empty (name)
  `(setf (slot-value ($e ,name) 'inner-h-t-m-l) ""))

;; when we want to execute these things on the server we can easily wrap the JS code like this...
;; !!! It might be useful to have a variant of this which spits JS out into the normal output stream. Maybe.
(defun ajax-js (js)
  (let ((js (if (stringp js)
                js (js:ps* js))))
    (ajax-render (mhtml js))))


;; If we need to be able to redraw something this is useful...
;; (I'd prefer to avoid explicit redraws wherever possible though)
;; this can be used to arbitrarily style things
(defun draw-context (name monad &key (type :div) style class redraw draggable width height src hidden)
  (with-web-monad

    value <- (melement monad
               (<> type :id (query-parameter-name name)
                   :class class
                   :src src
                   :style (if hidden
                              (append style (list :display "none"))
                              style)
                   :draggable (when draggable "true")
                   :width width :height height ; occasionally these are needed - eg for canvas
                   (output-html-events)
                   (dhtml (draw it))))
    
    ;; I've simplified this redraw protocol 
    - (if* (as-web-monad redraw) (redraw name monad))
    
    (unit value)))


;; This is useful for showing or hiding things etc...
;; actually I'm not sure if this will be useful. I  haven't used it yet
(defun set-client-flag (name)
  (mhidden-field (list "_FLAG" name) :default "yes"))

(defun is-client-flag-set (name)
  (mquery (list "_FLAG" name)))



(defun slidable (name monad &key style collapsed)
  (draw-context name monad :style (append style '(:overflow "hidden")
                                          (when collapsed '(:height "0px")))))

;; simple things to fade/slide things into view.
;; Next I would like to deal with cross fade or fade in/fade out whilst loading.

;; I would like fade in and swooshy things...
;; !!! Not working in IE
(defun fade-in (monad)
  (with-ids (fader)
    (with-web-monad
      (melement monad
        (:div :style (css :opacity "0"
                          :filter "alpha(opacity=0)"
                          :zoom 1
                          )
              :id fader
              ;; :onload (js (js-animate fader :transition 'glow.anim.fade-in))
              (draw it))
        (:script "glow.anim.fadeIn('#" fader "', 0.2);")))))

(defun slide-down (monad)
  (with-ids (slider)
    (with-web-monad
      (melement monad
        (:div :style (css :height (px 0) :overflow "hidden")
              :id slider
              (draw it))
        ;; (:script "window.onload=function() {glow.anim.slideDown('#" slider "', 0.2);};")
        (:script "glow.anim.slideDown('#" slider "', 0.2);")
        ))))


;; To make it easy to do hover styles I can use this. I could try and factor this out in generated code somehow, but it seems useful to me to do it this way. Modifying a global stylesheet somewhere is a pain.
(defun hover-styles (object hover normal)
  (flet ((pair-list (list)
           (loop for (key value) on list by #'cddr collect (list key value)))
         (handler (event)
           (lambda (style)
             (event-handler object event (js.set-slot-value object
                                                            (intern (s "STYLE.~A" (first style)) :keyword)
                                                            (second style))))))
    (with-web-monad
      - (mmapcar :web-monad
                 (handler :onmouseover)
                 (pair-list hover))

      (mmapcar :web-monad
               (handler :onmouseout)
               (pair-list normal)))))

;; deferred loading...
;; Important note - it's important that the time consuming bit is not at the top of a bind otherwise the whole page load will be delayed by this.
;; otherwise though, we're good to go.
;; why doesn't this one work but the next one does?
#+nil(defun deferred-load (monad)
  (with-ids (container)
    (let ((event :deferred-load))
      (with-web-monad
        (monadic progn
                 ;; This causes the monad to be executed when we trigger an AJAX event because it puts it into the AJAX stream only.
                 ;; The only other thing we have to do is to trigger an AJAX event...
                 (if* (mquery (list "__event" container event))
                      (ajax-load container monad))
                 (mhtml (:div :id container)
                        (:script (:print (js:ps* (js.ajax-event container event))))))))))

;; deferred loading...
;; Important note - it's important that the time consuming bit is not at the top of a bind otherwise the whole page load will be delayed by this.
;; otherwise though, we're good to go.
;; NOTE: This has never been used and gives compiler warnings
#+nil(defun deferred-load (monad &key (use-own-context t))
  (with-ids (container)
    (let ((event :deferred-load))
      (with-web-monad
        (flet ((monad (context)
                 (monadic progn
                          ;; This causes the monad to be executed when we trigger an AJAX event because it puts it into the AJAX stream only.
                          ;; The only other thing we have to do is to trigger an AJAX event...
                          (if* (mquery (list "__event" container event))
                               (ajax-load container monad))
                          (mhtml (:div :id container)
                                 (:script (:print (js:ps* (js.ajax-event container event :handler context))))))))
          (if use-own-context
              (abel-ajax-context context (monad context))
              (monad nil)))))))


;; NOTE - this does an async cross fade. In other words, it doesn't wait for fade out to finnish before fading in the new content. Most of the time that won't matter of course.
;; !!! Unused
(defun cross-fade (object event context content)
  (with-web-monad
    - (event-handler object event (js.animate context :transition 'glow.anim.fade-out))
    (if* (ajax-event object event)
         (mprogn (redraw context
                         content)
                 (ajax-js (js.animate context :transition 'glow.anim.fade-in))))))

;; Should it have a name so that we can control it from elsewhere? Probably.
;; I could always nest it...
;; This needs extending a bit, but it works



;; this is a very simple thing that just puts in a constant value and draws it too...
(defun html-value (x)
  (with-web-monad
    - (mhtml x)
    (unit x)))

;; I think I'm going to spend a lot of time making tables in the output, so I will make these simple functions here.

;; I think this would be useful...
(defmethod table-cell ((monad web-monad) &key style colspan rowspan class id sort)
  (let ((name id))
    (melement monad
      (<> :td :style style :colspan colspan :rowspan rowspan :class class
          :id id
          :sort sort
          (output-html-events)
          (dhtml (draw it))))))

(defmethod table-cell ((self t) &key style colspan rowspan class id)
  (table-cell (mhtml self)
              :style style :colspan colspan :rowspan rowspan :class class :id id))

;; this will have to contain a table-row
(defun table-head (monad)
  (melement monad
    (:thead (draw it))))

(defun table-row (monad &key style (valign "top"))
  (melement monad
    (<> :tr :style style :valign valign
        (dhtml (draw it)))))

(defun table (monad &key class style (sortable t))
  (melement monad
    (<> :table :style style
        :id (when sortable (s "_~A" (random-string 10)))
        :class (if (and sortable class)
                   (s "sortable ~A" class)
                   (or class
                       (when sortable "sortable")))
        (dhtml
          (draw it)))))

;; !!! I could very easily make a nice table widget that would trivialise things like the database-table function and the CSV import table widget BUT make it editable. It would have a value of a list of rows. It could work as a general editor for tabulated data.

;; in fact, I could, perhaps, make a general spreadsheet if I wanted
;; to (although that might be of limited use). Still, things like
;; rarranging columns etc could be useful. To begin with a general
;; table editor would be good.



;; !!! NEEDED

;; 1. drag and drop (made simple)
;; 2. dynamic values for widgets
;; 3. the rest of the event stuff (character codes and event parameters)
;; 4. dimmer that works in IE as a simple monadic function

;; !!! The only case where this doesn't work is when the user resizes the window in IE.
;; this just outputs correct, standard compliant HTML and then a bit of code to fix IE
(defun dimmer (monad)
  (with-ids (dimmer)
    (with-web-monad
      - (mhtml (:div :style (css :position "fixed" :top (px 0) :left (px 0)
                                 :width "100%"
                                 :height "100%"
                                 :filter "alpha(opacity=50)"
                                 :background-color "rgba(0,0,0,0.5)")
                     :id dimmer)
               (html (:script (:noescape
                               (:print
                                (js:ps* `(if (in-i-e)
                                             (flet ((max (a b)
                                                      (return (if (> a b)
                                                                  a b))))
                                               (let ((d ($ ,dimmer)))
                                                 (setf d.style.position "absolute"
                                                       d.style.top "0px"
                                                       d.style.left "0px"
                                                       d.style.width (+ "" document.body.client-width "px")
                                                       d.style.height (+ "" (max document.body.client-height
                                                                                 document.document-element.client-height) "px")

                                                       d.style.background-color "black"))))))))))
      
      (progn monad))))

;; If we follow the convention that a widget will return :error in the event that it has been given an invalid value then we might sometimes want to ignore the error and just consider its value to be nil - as if nothing had been entered. This function will stop us having to error check the value from each function. There should be a better way of doing this I think. I don't want to do something like maybe though.

;; maybe I could make an error path - keep track of errors in the monadic result as we go along. Hmmm - interesting idea. Then we would have a (validation-errors) monadic function which would return a list of which things generated validation errors. Sounds like a plan.

;; it could even be used as a more general way of trapping errors perhaps




;; These 2 use the event mechanism to output named data which can then be displayed EARLIER if required 

(defun set-monad-data (name data)
  (event-handler name :data data))

(defmacro get-monad-data (name)
  ;; `(mapcar 'event-handler-action (events-for-object ,name +event-handlers))
  `(event-handler-action (first (events-for-object ,name +event-handlers))))




;; This is just looking at values at the moment
;; it would be good if it looked at arbitrary JS expressions
(defun compound-change-handler (name values)
  (mhtml
    (let ((events (remove :onchange
                          (remove name
                                  web-monad::+event-handlers
                                  :test-not #'equal
                                  :key #'event-handler-object)
                          :test-not #'eq
                          :key #'event-handler-event)))
      (when events
        (html
          (:noescape
           (:script
            "crun(crepeat(200,false,'" (:print (query-parameter-name name)) "').select(function() {return "
            ;; now I have to get the values that I want to trigger the change...
            (html (:print (ps:ps* (cons '+
                                        (loop for a in values
                                              collect `(slot-value ($e ,a) 'value)
                                              collect "///")))))
               
            "}).bind(distinct()).select(function(value) {"
            ;; "console.log('Value is now: '+value+'.');"
            (loop for (nil b) on (output-event-list events)
                  by #'cddr
                  do (html b))
            "}))")))))))

;; Sticks 'the' in front of the input descriptions
;; might seem frivolous, but maybe it'll be handy. It works with the grammar constructions here
(defun the-input (values)
  (check-type values list)
  (mapcar (lambda (item)
            (check-type item (vip-utils::tuple string t))
            (list (format nil "the ~A" (string-downcase (first item)))
                  (second item)))
          values))


