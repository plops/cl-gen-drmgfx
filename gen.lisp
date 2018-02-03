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
		  (include <cstring>)
		  (include <sys/mman.h>)
		  (include <unistd.h>)

		  (raw " ")
		  (include <xf86drm.h>)
		  (include <xf86drmMode.h>)

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
			    
			    (let ((fd :type int :init (funcall drmOpen (string "i915") nullptr)))
			      (if (< fd 0)
				  (macroexpand (er "drmOpen error: " fd)))
			      (let ((r :init (funcall drmSetClientCap fd DRM_CLIENT_CAP_UNIVERSAL_PLANES 1)))
				(if (!= 0 r)
				    (macroexpand (er "drmSetClientCap error: " r))))
			      (let ((res :init  (funcall drmModeGetResources fd)))
				(funcall assert res)
				(let ((c :type drmModeConnectorPtr :init nullptr))
				 (dotimes (i res->count_connectors)
				   (setf c  (funcall drmModeGetConnector fd (aref res->connectors i)))
				   (funcall assert c)
				   (if (== DRM_MODE_CONNECTED
					   c->connection)
				       (break))
				   (funcall drmFree c))
				 (let ((enc :init (funcall drmModeGetEncoder fd c->encoder_id))
				       (crtc :init (funcall drmModeGetCrtc fd enc->crtc_id))
				       (fb :init (funcall drmModeGetFB fd crtc->buffer_id))
				       (plane_res :init (funcall drmModeGetPlaneResources fd))
				       (plane :type drmModePlanePtr :init nullptr))
				   (statements
				     ,@(loop for e in '(enc crtc fb plane_res) collect
					    `(funcall assert ,e)))
				   (dotimes (i plane_res->count_planes)
				     (setf plane (funcall drmModeGetPlane fd (aref plane_res->planes i)))
				     (funcall assert plane)
				     (if (== plane->fb_id fb->fb_id)
					 (break))
				     (funcall drmFree plane))
				   (let ((has_dumb :type uint64_t :init 0))
				     (funcall assert (! (funcall drmGetCap fd DRM_CAP_DUMB_BUFFER &has_dumb)))
				     (funcall assert has_dumb))
				   (let ((creq :type "struct drm_mode_create_dumb"
					       :init (list .width=fb->width
							   .height=fb->height
							   .bpp=fb->bpp)))
				     (funcall assert (! (funcall drmIoctl fd DRM_IOCTL_MODE_CREATE_DUMB &creq)))
				     )))))
			    
			    
			    
			    (return 0)))))
    (write-source "stage/cl-gen-drmgfx/source/main" "cpp" code)))


