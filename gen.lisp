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
					
		  (include <cstdint>)
		  (include <cassert>)
		  (include <sys/types.h>)
		  (include <sys/stat.h>)
		  (include <fcntl.h>)

		  (include <sys/ioctl.h>)
		  
		  (raw " ")
		  (include <drm/drm.h>)
		  (raw "// drm/drm.h needs to be patched for C++ to compile, one of the datastructures has an element called virtual")
		  (include <drm/drm_mode.h>)
		  (include <fstream>)
		  (include <string>)
		  (include <vector>)
		  (include <cstring>) ;; memset
		  (include "cxxopts.hpp")
		  (include <iomanip>) ;; setw
		  (include <array>)
		  (raw " ")
		  
		  (raw " ")
		  (raw "//! \\mainpage Draw using direct rendering manager")
		  (raw "//! \\section Introduction")
		  (raw "//! This repository contains a minimal program to draw to a linux screen. See http://betteros.org/tut/graphics1.php for a tutorial this code is based on.")
		  (raw "//! \\section Dependencies ")
		  (raw "//! - Linux kernel with DRM driver")
		  (raw " ")
		  (raw "//! - cxxopts for commandline argument parsing.")
		  (raw " ")
		  (raw "//! - sbcl to generate c++ code")
		  (raw "//! - cmake to configure for build")
		  (raw "//! - g++ to compile c++ code")
		  (raw " ")
		  (raw "//! - For the documentation (optional for use):")
		  (raw "//!   + doxygen")
		  (raw " ")
		  (raw "//! - The C++ code can be opened as a cmake project in kdevelop, clion or qtcreator (order of my preference).")
		  (raw " ")
		  (raw "//! \\section References ")
		  ,@(loop for i from 1 and e in '("http://betteros.org/tut/graphics1.php")
		       collect
			 `(raw ,(format nil "//! ~a. ~a" i e)))
		  
		  
		  ,@(dox :brief "main function"
			 :usage "parse command line parameters and draw to screen"
			 :params '((argc "input number of command line arguments")
				   (argv "input"))
			 :return "Integer")
		  		  
		  (function (main ((argc :type int)
				   (argv :type char**)) int)

			    
			    
			    
			    (with-compilation-unit 
				(raw "try")
			      (let ((options :type "cxxopts::Options" :ctor (comma-list (string "drm_draw") (string "draw to drm device"))))
				(with-compilation-unit
				    (raw "options.add_options()")
				  (raw "(\"h,help\", \"Print help\")")
				  (raw "(\"d,device\",\"device file\",cxxopts::value<std::string>()->default_value(\"/dev/dri/card0\"))")
				  (raw "(\"r,rate\",\"frame rate (Hz,double)\",cxxopts::value<double>()->default_value(\"60.0\"));")
				  )
				(funcall options.parse argc argv)
				
				(if (funcall options.count (string "help"))
				    (statements
				     (macroexpand (e (funcall options.help)))
				     (funcall exit 0)))
				(macroexpand (e "requested frame rate = "
						(funcall "options[\"rate\"].as<double>")
						" Hz device="
						(funcall "options[\"device\"].as<std::string>")
						))
				(let ((dri_fd :ctor (funcall open "options[\"device\"].as<std::string>().c_str()" O_RDWR)))
				  (macroexpand (checked-ioctl dri_fd DRM_IOCTL_SET_MASTER 0))
				  (let ((res{} :type drm_mode_card_res))
				    (funcall memset &res 0 (funcall sizeof res)) ;; fixme is this required?
				    (macroexpand (checked-ioctl dri_fd DRM_IOCTL_MODE_GETRESOURCES &res))
				    (statements ,@(loop for i in '(count_fbs count_crtcs count_connectors count_encoders)
					  appending
					    `((macroexpand (e ,(format nil "~a = " i)
							      (slot-value res ,i)))
					      (funcall assert (< (slot-value res ,i) 10)))))
				    (statements ,@(loop for i in '(min_width max_width min_height max_height)
					  appending
					    `((macroexpand (e ,(format nil "~a = " i)
							      (slot-value res ,i)))))))
				  (let ((resources{} :type drm_mode_card_res))
				    ,@(loop for i in '(fb crtc connector encoder) appending
					   `((decl ((,(format nil "~a_array{}" i) :type "std::array<uint64_t,10>")))
					     (setf (slot-value resources ,(format nil "~a_id_ptr" i))
						   (funcall reinterpret_cast<uint64_t>
							    ,(format nil "~a_array.data()" i)))))
				    (macroexpand (checked-ioctl dri_fd DRM_IOCTL_MODE_GETRESOURCES &resources))
				    (dotimes (i resources.count_connectors)
				      (let ((connector{} :type drm_mode_get_connector))
					(funcall memset &connector 0 (funcall sizeof connector)) ;; fixme is this required?
					(setf connector.connector_id (funcall connector_array.at i))
					(macroexpand (checked-ioctl dri_fd DRM_IOCTL_MODE_GETCONNECTOR &connector))
					(if (\|||\|
					       (\|||\|
						  (< connector.count_encoders 1)
						  (< connector.count_modes 1))
					       (\|||\|
						  (! connector.encoder_id)
						  (! connector.connection)))
					    (statements
					     (raw "continue;")))
					(statements ,@(loop for i in '(count_modes count_props count_encoders)
							 appending
							   `((macroexpand (e ,(format nil "~a = " i)
									     (slot-value connector ,i)))
							     (funcall assert (< (slot-value connector ,i) 20)))))
					(setf connector.connector_id (funcall connector_array.at i))
					,@(loop for i in '(modes) appending
					       `((decl ((,(format nil "~a_array{}" i) :type "std::array<struct drm_mode_modeinfo,20>")))
						 (setf (slot-value connector ,(format nil "~a_ptr" i))
						       (funcall reinterpret_cast<uint64_t>
								,(format nil "~a_array.data()" i)))))
					,@(loop for i in '(props prop_values encoders) appending
					       `((decl ((,(format nil "~a_array{}" i) :type "std::array<uint64_t,20>")))
						 (setf (slot-value connector ,(format nil "~a_ptr" i))
						       (funcall reinterpret_cast<uint64_t>
								,(format nil "~a_array.data()" i)))))
					(macroexpand (checked-ioctl dri_fd DRM_IOCTL_MODE_GETCONNECTOR &connector))
					
					)))))
			      
			      (raw "catch (const cxxopts::OptionException& e)")
			      (let ()
				(macroexpand (e "error parsing options: " (funcall e.what)))
				(funcall exit 1)))
			    
			    (return 0)))))
    (write-source "stage/cl-gen-graphics-drm/source/main" "cpp" code)))


