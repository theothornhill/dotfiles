(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:yason :drakma :flexi-streams :unix-opts :cl-ppcre :alexandria) :silent t))

(defpackage :weather
  (:use :cl)
  (:local-nicknames (#:a #:alexandria))
  (:export :toplevel))

(in-package :weather)

(defvar *weather* nil)
(defvar *default-offset* 3)

(defclass weather ()
  ((response :initarg :response
             :accessor response)
   (geometry :initarg :geometry
             :accessor geometry)
   (properties :initarg :properties
               :accessor properties)
   (units :initarg :units
          :accessor units)
   (updated-at :initarg :updated-at
               :accessor updated-at)
   (timeseries :initarg :timeseries
               :accessor timeseries)))

(defun decode-json (response)
  (let* ((data (yason:parse response))
         (properties (gethash "properties" data)))
    (make-instance 'weather
                   :response data
                   :geometry (gethash "geometry" data)
                   :properties properties
                   :units (gethash "meta" properties)
                   :updated-at (gethash "updated_at" properties)
                   :timeseries (gethash "timeseries" properties))))


(defun format-time (time)
  "Strip out the HH:mm part of an ISO 8601 string."
  (declare (type string time))
  (subseq time 11 16))

(defun remove-underscores (weather)
  (cl-ppcre:regex-replace-all "_" weather " "))

(defun format-weather-string (weather)
  (a:switch (weather :test #'equalp)
    ("clearsky_day" "clear sky")
    ("clearsky_night" "clear sky")
    ("clearsky_polartwilight" "clear sky")
    ("fair_day" "fair")
    ("fair_night" "fair")
    ("fair_polartwilight" "fair")
    ("lightssnowshowersandthunder_day" "light snow showers and thunder")
    ("lightssnowshowersandthunder_night" "light snow showers and thunder")
    ("lightssnowshowersandthunder_polartwilight" "light snow showers and thunder")
    ("lightsnowshowers_day" "light snow showers")
    ("lightsnowshowers_night" "light snow showers")
    ("lightsnowshowers_polartwilight" "light snow showers")
    ("heavyrainandthunder" "heavy rain and thunder")
    ("heavysnowandthunder" "heavy snow and thunder")
    ("rainandthunder" "rain and thunder")
    ("heavysleetshowersandthunder_day" "heavy sleet showers and thunder")
    ("heavysleetshowersandthunder_night" "heavy sleet showers and thunder")
    ("heavysleetshowersandthunder_polartwilight" "heavy sleet showers and thunder")
    ("heavysnow" "heavy snow")
    ("heavyrainshowers_day" "heavy rain showers")
    ("heavyrainshowers_night" "heavy rain showers")
    ("heavyrainshowers_polartwilight" "heavy rain showers")
    ("lightsleet" "light sleet")
    ("heavyrain" "heavy rain")
    ("lightrainshowers_day" "light rain showers")
    ("lightrainshowers_night" "light rain showers")
    ("lightrainshowers_polartwilight" "light rain showers")
    ("heavysleetshowers_day" "heavy sleet showers")
    ("heavysleetshowers_night" "heavy sleet showers")
    ("heavysleetshowers_polartwilight" "heavy sleet showers")
    ("lightsleetshowers_day" "light sleet showers")
    ("lightsleetshowers_night" "light sleet showers")
    ("lightsleetshowers_polartwilight" "light sleet showers")
    ("snow" "snow")
    ("heavyrainshowersandthunder_day" "heavy rain showers and thunder")
    ("heavyrainshowersandthunder_night" "heavy rain showers and thunder")
    ("heavyrainshowersandthunder_polartwilight" "heavy rain showers and thunder")
    ("snowshowers_day" "snow showers")
    ("snowshowers_night" "snow showers")
    ("snowshowers_polartwilight" "snow showers")
    ("fog" "fog")
    ("snowshowersandthunder_day" "snow showers and thunder")
    ("snowshowersandthunder_night" "snow showers and thunder")
    ("snowshowersandthunder_polartwilight" "snow showers and thunder")
    ("lightsnowandthunder" "light snow and thunder")
    ("heavysleetandthunder" "heavy sleet and thunder")
    ("lightrain" "light rain")
    ("rainshowersandthunder_day" "rain showers and thunder")
    ("rainshowersandthunder_night" "rain showers and thunder")
    ("rainshowersandthunder_polartwilight" "rain showers and thunder")
    ("rain" "rain")
    ("lightsnow" "light snow")
    ("lightrainshowersandthunder_day" "light rain showers and thunder")
    ("lightrainshowersandthunder_night" "light rain showers and thunder")
    ("lightrainshowersandthunder_polartwilight" "light rain showers and thunder")
    ("heavysleet" "heavy sleet")
    ("sleetandthunder" "sleet and thunder")
    ("lightrainandthunder" "light rain and thunder")
    ("sleet" "sleet")
    ("lightssleetshowersandthunder_day" "light sleet showers and thunder")
    ("lightssleetshowersandthunder_night" "light sleet showers and thunder")
    ("lightssleetshowersandthunder_polartwilight" "light sleet showers and thunder")
    ("lightsleetandthunder" "light sleet and thunder")
    ("partlycloudy_day" "partly cloudy")
    ("partlycloudy_night" "partly cloudy")
    ("partlycloudy_polartwilight" "partly cloudy")
    ("sleetshowersandthunder_day" "sleet showers and thunder")
    ("sleetshowersandthunder_night" "sleet showers and thunder")
    ("sleetshowersandthunder_polartwilight" "sleet showers and thunder")
    ("rainshowers_day" "rain showers")
    ("rainshowers_night" "rain showers")
    ("rainshowers_polartwilight" "rain showers")
    ("snowandthunder" "snow and thunder")
    ("sleetshowers_day" "sleet showers")
    ("sleetshowers_night" "sleet showers")
    ("sleetshowers_polartwilight" "sleet showers")
    ("cloudy" "cloudy")
    ("heavysnowshowersandthunder_day" "heavy snow showers and thunder")
    ("heavysnowshowersandthunder_night" "heavy snow showers and thunder")
    ("heavysnowshowersandthunder_polartwilight" "heavy snow showers and thunder")
    ("heavysnowshowers_day" "heavy snow showers")
    ("heavysnowshowers_night" "heavy snow showers")
    ("heavysnowshowers_polartwilight" "heavy snow showers")))

(defun format-weather-instant-verbose (hour)
  (let* ((data (gethash "data" hour))
         (time (gethash "time" hour))
         (instant (gethash "instant" data))
         (details (gethash "details" instant)))
    (format t "~a~%" (format-time time))
    (maphash (lambda (k v)
               (let* ((units (gethash "units" (units *weather*)))
                      (unit (gethash k units)))
                 (format t "~@(~40a~): ~a ~a~%"
                         (remove-underscores k)
                         v
                         unit)))
             details)
    (format t "~%")))

(defun format-weather-instant (hour)
  (let* ((data (gethash "data" hour))
         (time (gethash "time" hour))
         (instant (gethash "instant" data))
         (next-hour (gethash "next_1_hours" data))
         (details (gethash "details" instant))
         (summary (gethash "summary" next-hour))
         (symbol-code (gethash "symbol_code" summary))
         (temperature (gethash "air_temperature" details)))
    (format t "~a: It is ~a degrees outside, and it is ~a~%"
            (format-time time)
            temperature
            (format-weather-string symbol-code))))



(defun parse-float (number-string)
  (with-input-from-string (in number-string)
    (read in)))

(defun parse-line (line)
  (let* ((split (cl-ppcre:split "," (cl-ppcre:regex-replace-all "\"" line "")))
         (city-name-dashed (cl-ppcre:regex-replace-all " " (second split) "-"))
         (city (intern (string-upcase city-name-dashed) :keyword))
         (latitude (parse-float (third split)))
         (longitude (parse-float (fourth split))))
    (list city (cons latitude longitude))))

(defun read-cities ()
  "Turn the worldcities csv into a list.

Each element takes the form of (:TROFIMOVSK (72.5997 . 127.0337)).  Cities with
\"'\" in its name, like \"ust'-nyukzha\" will parse like this:
(:|UST'-NYUKZHA| (56.5312 . 121.6131)).
"
  (with-open-file
      (file "~/dotfiles/lisp/data/worldcities.csv")
    (read-line file) ; Drop the first line
    (loop for line = (read-line file nil nil)
          while line
          collect (parse-line line))))

(defun find-city-lat-long (target)
  "Find a city in a list of cities, and return its latitude/longitude.

One city is on the form (:TROFIMOVSK (72.5997 . 127.0337))"
  (let ((city (find-if (lambda (city)
                         (eq (car city) target))
                       (read-cities))))
    (getf city target)))


(defun get-from-met (town)
  "Sends http request to api.met.no to fetch the weather.
Sets latitude and longitude in request url."
  (with-input-from-string
      (s (flexi-streams:octets-to-string
          (drakma:http-request
           (concatenate
            'string
            "https://api.met.no/weatherapi/locationforecast/2.0/compact?"
            (format nil "lat=~a" (car town))
            (format nil "&lon=~a" (cdr town))))))
    (decode-json s)))

(defun timeseries-offset (timeseries offset)
  "Pick out the nth element in TIMESERIES specified by OFFSET."
  (if (< offset (length timeseries))
      (nth offset timeseries)
      (error "Not possible to offset this much")))

(defun get-weather (city &key
                           (offset *default-offset*)
                           (range 1)
                           (format-fn #'format-weather-instant))
  "Get the verbose information about the weather this instant in UTC."
  (a:when-let* ((town (find-city-lat-long city))
                (*weather* (get-from-met town)))
    (dotimes (r range)
      (funcall format-fn (timeseries-offset (timeseries *weather*) (+ offset r))))))

(defun get-weather-verbose (city &key
                                   (offset 0)
                                   (range 1))
  (get-weather city :offset offset
                    :range range
                    :format-fn #'format-weather-instant-verbose ))

(defun now (&key (city :bergen))
  (get-weather city :offset 1))

(defun parse-number-arg (arg)
  (abs (parse-integer arg :junk-allowed t)))

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :city
   :description "The city you want weather for"
   :arg-parser #'identity
   :short #\c
   :long "city"
   :meta-var "CITY")
  (:name :verbose
   :description "Show more details"
   :short #\v
   :long "verbose")
  (:name :offset
   :description "Offset of 'N' hours"
   :arg-parser #'parse-number-arg
   :short #\o
   :long "offset"
   :meta-var "N")
  (:name :range
   :description "Offset of 'N' hours"
   :arg-parser #'parse-number-arg
   :short #\r
   :long "range"
   :meta-var "N"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro with-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun show-help ()
  (opts:describe
   :prefix "Get the temperature in a given city"
   :args "cityname")
  (uiop:quit))

(defun arg-as-keyword (arg)
  (let ((city-name-dashed
          (cl-ppcre:regex-replace-all " " arg "-")))
    (intern (string-upcase city-name-dashed) :keyword)))

(defun run-with-city (city options)
  (let ((verbose (getf options :verbose))
        (offset (or (getf options :offset) *default-offset*))
        (range (or (getf options :range) 1)))
    (cond
      (verbose
       (get-weather
        (arg-as-keyword city)
        :offset offset
        :range range
        :format-fn #'format-weather-instant-verbose))
      (t
       (get-weather
        (arg-as-keyword city)
        :offset offset
        :range range)))))

(defun toplevel ()
  (multiple-value-bind (options)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (condition)
          (format t "fatal: ~a~%" condition)
          (opts:exit 1)))
    (when (null options)
      (run-with-city "bergen" options)
      (uiop:quit))
    (with-option (options :help) (show-help))
    (with-option (options :city)
      (run-with-city it options)))
  (uiop:quit))
