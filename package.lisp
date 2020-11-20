
(in-package :cl-user)

(defpackage :web-monad
  (:use :cl :vip-utils :monads :net.aserve :com.gigamonkeys.html :anaphors :cl-ppcre
        :web-utils)
  (:export
   ;; quite a few things...

   #:*m-entity*
   #:*monad-dispatch-table*
   #:*monadic-event-handlers*
   #:*monadic-handler-check-functions*

   #:check-monadic-type
   #:typed-bind

   #:ajax-drag-drop
   #:ajax-event
   #:ajax-file-drop          ; only used in rdf-export-and-import.lisp
   #:ajax-js
   #:ajax-load ; only used in bundling.lisp
   #:ajax-render
   #:alert
   #:bookmark-page                     ; used in excess-guarantee.lisp
   #:conditionally-visible
   #:currency-formatter ; maybe these formatters could be split out? Probably redundant now though
   #:current-monad-id
   #:date-formatter
   #:dimmer
   #:dont-render
   #:draw-context
   #:drop-zone-highlight
   #:editable-image
   #:end-request
   #:error-page

   #:event-handler-action
   #:event-handler-context
   #:event-handler-event
   #:event-handler-object
   #:event-handler-p
   #:events-for-object
   #:events-of-type
   #:expire-old-monads

   #:fade-in
   #:file-drop
   #:hide/show-button
   #:hover-styles  ; only used in bundling.lisp
   #:html-value
   #:image-paste-handler

   #:js.animate
   #:js.fade-out-element  ; only used in bundling.lisp
   #:js.set-slot-value
   #:js.ajax-event

   #:make-event-handler
   #:make-monad-url-handler
   #:make-web-monad
   #:make-web-thunk

   #:mbase-uri
   #:mbounce                            ; unused
   #:mbutton
   #:mcheckbox
   #:mheader-slot
   #:mhidden-field
   #:mhost-uri
   #:mhtml-from-template
   #:mhttp-redirect
   #:mhttp-response
   #:minimum-length-formatter
   #:minput
   #:mlink-to
   #:mmonadic-post-handler
   #:mmulti-pane-form
   #:mnew-reply
   #:monad-dispatch-id
   #:monad-redirect
   #:monad-thunk
   #:monad-url-handler
   #:monad-url-handler-last-access-time
   #:monad-url-handler-lifespan
   #:monad-url-handler-monad
   #:monad-url-handler-name
   #:monad-url-handler-one-shot
   #:monad-url-handler-p
   #:monad-url-handler-user
   #:monadic-post-handler
   #:mone-of
   #:mpost-p
   #:mquery
   #:mradio-buttons
   #:mrequest
   #:mrequest-uri
   #:msearchable-drop-down
   #:mselect
   #:msession-datum
   #:mshow
   #:msubmit
   #:msubmit-link
   #:mtextarea
   #:multi-value-input
   #:mupdatable                         ; unused, but could be
   #:muser-account
   #:mweb-page

   ;; these ones are not used externally at the moment, but were used to implement the
   ;; msearchable-drop-down. I quite like the idea though.
   #:mwidget
   #:mwidget-mouse-events
   #:mwidget-scroll
   #:named-capability
   #:non-empty-formatter
   #:number-formatter
   #:output-event-list
   #:output-events-for-object
   #:parsed-date
   #:popup-dialog
   #:redraw
   #:regex-formatter
   #:register-monadic-handler
   #:render-web-thunk
   #:report-error
   #:set-client-flag                    ; unused. Could it be?
   #:set-monad-data
   #:slidable
   #:slide-down
   #:strip-space-formatter

   #:table
   #:table-head
   #:table-row
   #:testable-query-value
   #:uk-postcode-formatter
   #:unregister-monad
   #:url
   #:value-and-render
   #:web-monad-handle-request
   #:web-monad-handler
   #:web-monad-p
   #:web-monad-run
   #:web-thunk-ajax-render
   #:web-thunk-event-handlers
   #:web-thunk-new-page-p
   #:web-thunk-p
   #:web-thunk-render
   #:web-thunk-stop
   #:web-thunk-value
   #:draw-thunk
   #:get-monad-data
   #:melement
   #:mhtml
   #:mhtml*
   #:mhtml-let
   #:output-html-events
   #:in-div

   #:with-web-monad
   #:event-handler
   #:as-web-monad
   #:is-monadic-value
   #:table-cell


   #:draw
   #:value
   #:mvalue
   #:web-thunk
   #:web-monad
   
   #:+request
   #:+event-handlers
   #:yield
   
   #:pane-stream
   #:mgroovy-pane-form
   #:current-pane-stream-page

   #:js.disabled
   #:js.empty
   #:the-input
   #:validate-input))

