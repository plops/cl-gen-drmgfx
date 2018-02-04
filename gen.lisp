(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro er (&body body)
  `(statements (<< "std::cerr" ,@(loop for e in body collect
                                      (cond ((stringp e) `(string ,e))
                                            (t e))) "std::endl")))

(defmacro checked-ioctl (fd cmd arg)
  `(if (< (funcall ioctl ,fd ,cmd ,arg) 0)
     (macroexpand (er ,(format nil "ioctl ~a failed." cmd)))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun dox (&key brief usage params return)
  `(
    (raw " ")
    (raw ,(format nil "//! @brief ~a" brief)) (raw "//! ")
    (raw ,(format nil "//! @usage ~a" usage)) (raw "//! ")
    ,@(loop for (name desc) in params collect
         `(raw ,(format nil "//! @param ~a ~a" name desc)))
    (raw "//! ")
    (raw ,(format nil "//! @return ~a" return))
    (raw " ")))


(defparameter *drm-facts*
  `((10 "")))


(defmacro with-open-fstream ((f fn &key (dir "/dev/shm")) &body body)
  `(let ((,f :type "std::ofstream" :ctor (comma-list (string ,(format nil "~a/~a" dir fn)))))
     ,@body))

(progn
  (let* ((code `(with-compilation-unit
		    (with-compilation-unit
			(raw "//! \\file main.cpp Draw to screen using linux direct rendering manager"))

		  (include <iostream>)
		  (include <cassert>)
		  (include <cstdlib>)
		  (include <errno.h>)
		  (include <cstring>)
		  (include <sys/mman.h>)
		  (include <unistd.h>)

		  (raw " ")
		  (include <xf86drm.h>)
		  (include <xf86drmMode.h>)
		  (include <i915_drm.h>)

		  (raw "//! This repository contains a minimal program to draw to a linux screen.")
		  (raw "//! \\section Dependencies ")
		  (raw "//! - Linux kernel with DRM driver")
		  (raw "//! - libdrm")
		  (raw " ")
		  (raw "//! - sbcl to generate c++ code")
					;(raw "//! - cmake to configure for build")
		  (raw "//! - g++ to compile c++ code")
		  (raw " ")
		  (raw "//! - For the documentation (optional):")
		  (raw "//!   + doxygen")
		  (raw " ")
		  
		  (raw " ")
		  (raw "//! \\section References ")
		  ,@(loop for i from 1 and e in '("http://betteros.org/tut/graphics1.php")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))
		  
		  
		  ,@(dox :brief "main function"
			 :usage "draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)
			    (if (== 0 (funcall drmAvailable))
				(macroexpand (er "drm not available")))
			    
			    (let ((errno :type
				    "extern int")
				  (fd :init (paren-list (let ((fd :init (funcall drmOpen (string "i915") nullptr)))
							  (if (< fd 0)
							      (macroexpand (er "drmOpen error: fd=" fd " errno=" errno)))
							  (let ((rr :init (funcall drmSetClientCap fd DRM_CLIENT_CAP_UNIVERSAL_PLANES 1)))
							    (if (!= 0 rr)
								(macroexpand (er "drmSetClientCap error: " rr " errno=" errno))))
							  (raw "fd"))))
				  (res :init  (paren-list (let ((r :init (funcall drmModeGetResources fd)))
							    (funcall assert r)
							    (raw "r"))))
				  (c :type drmModeConnectorPtr
				     :init (paren-list
					    (let ((c :type drmModeConnectorPtr :init nullptr))
					      (dotimes (i (funcall "static_cast<unsigned int>" res->count_connectors))
						(setf c  (funcall drmModeGetConnector fd (aref res->connectors i)))
						(funcall assert c)
						(if (== DRM_MODE_CONNECTED
							c->connection)
						    (break))
						(funcall drmFree c))
					      (raw "c"))))
				  (enc :init (funcall drmModeGetEncoder fd c->encoder_id))
				  (crtc :init (funcall drmModeGetCrtc fd enc->crtc_id))
				  (fb :init (funcall drmModeGetFB fd crtc->buffer_id))
				  (plane_res :init (funcall drmModeGetPlaneResources fd))
				  (plane  :init (paren-list
						 (let ((p :type drmModePlanePtr :init nullptr))
						   (statements
						    ,@(loop for e in '(enc crtc fb plane_res) collect
							   `(funcall assert ,e)))
						   (dotimes (i plane_res->count_planes)
						     (setf p (funcall drmModeGetPlane fd (aref plane_res->planes i)))
						     (funcall assert p)
						     (if (== p->fb_id fb->fb_id)
							 (break))
						     (funcall drmFree p))
						   (raw "p"))))
				  (has_dumb :init (paren-list
						   (let ((has_dumb :type uint64_t :init 0))
						     (funcall assert (! (funcall drmGetCap fd DRM_CAP_DUMB_BUFFER &has_dumb)))
						     (funcall assert has_dumb)
						     (raw "has_dumb"))))
				  (creq :init
				    (paren-list
				     (let ((creq :type "struct drm_mode_create_dumb"))
				       (funcall memset &creq 0 (funcall sizeof creq))
				       (setf creq.width fb->width
					     creq.height fb->height
					     creq.bpp fb->bpp)
				       (funcall assert (! (funcall drmIoctl fd DRM_IOCTL_MODE_CREATE_DUMB &creq)))
				       (macroexpand (e "width=" creq.width " height=" creq.height))
				       (raw "creq"))))
				  (my_fb :init (paren-list
						(let ((my_fb :type uint32_t :init 0))
						    (funcall assert
							     (! (funcall drmModeAddFB fd creq.width creq.height 24
									 creq.bpp creq.pitch creq.handle &my_fb)))
						    (raw
						     "my_fb"))))
				  (flip :init (paren-list
					       (let ((gp :type "struct drm_i915_getparam"
							 )
						     (value :type int))
						 (funcall memset &gp 0 (funcall sizeof gp))
						 (setf gp.param I915_PARAM_HAS_PAGEFLIPPING
						       gp.value &value)
						 (let ((r :init (funcall drmCommandWriteRead fd DRM_I915_GETPARAM
									 &gp (funcall sizeof gp))))
						   (if r
						       (macroexpand (er

								      "i915_getparam " r))))
						 (macroexpand (e
								"flipping=" *gp.value))
						 (raw "*gp.value"))))
				  (mreq :init (paren-list
					       (let ((mreq :type "struct drm_mode_map_dumb"))
						 (funcall memset &mreq 0 (funcall sizeof mreq))
						 (setf mreq.handle creq.handle)
						 (funcall assert
							  (! (funcall drmIoctl fd DRM_IOCTL_MODE_MAP_DUMB &mreq)))
						 (raw "mreq"))))
				  (map :init (paren-list
					      (let ((map :type uint32_t*
							 :init (funcall static_cast<uint32_t*>
									(funcall mmap 0 creq.size
											(\| PROT_READ PROT_WRITE)
											MAP_SHARED fd mreq.offset))))
						  (funcall assert (!= MAP_FAILED map))
						  (funcall memset map 0 creq.size)
						  (raw "map")))))     
			      
			      (dotimes (i 256)
				(dotimes (j 256)
				  (setf (aref map (+ j (* i (>> creq.pitch 2))))
					(hex #x12345678))))
			      (funcall assert (! (funcall drmModeSetCrtc fd crtc->crtc_id my_fb
							  0 0 &c->connector_id 1 &crtc->mode)))
			      (dotimes (k 256)
			       (dotimes (i creq.height)
				 (dotimes (j creq.width)
				   (setf (aref map (+ j (* i (>> creq.pitch 2))))
					 (+ k (hex #x12345678)))))
			       (funcall usleep 32000))

			      #+nil (funcall sleep 1)
			      (funcall assert (! (funcall drmModeSetCrtc fd crtc->crtc_id fb->fb_id
							  0 0 &c->connector_id 1 &crtc->mode)))
			      (funcall assert (! (funcall drmModeRmFB fd my_fb)))
			      (let ((dreq :type "struct drm_mode_destroy_dumb"))
				(funcall memset &dreq 0 (funcall sizeof dreq))
				(setf dreq.handle creq.handle)
				(funcall assert (! (funcall drmIoctl fd DRM_IOCTL_MODE_DESTROY_DUMB &dreq))))
			      (statements
			       ,@(loop for e in '(plane plane_res fb crtc enc c res) collect
				      `(funcall drmFree ,e)))
			      (funcall drmClose fd))
			    
			    
			    
			    
			    (return 0)))))
    (write-source "stage/cl-gen-drmgfx/source/main" "cpp" code)))


