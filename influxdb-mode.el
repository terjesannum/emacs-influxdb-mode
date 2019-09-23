(require 'comint)

(defvar influxdb-host (or (getenv "INFLUX_HOST") "localhost"))
(defvar influxdb-port 8086)
(defvar influxdb-database (or (getenv "INFLUX_DATABASE") "_internal"))
(defvar influxdb-precision "rfc3339")
(defvar influxdb-cli "/usr/bin/influx")
(defvar influxdb-mode-hook nil)
(defvar influxdb-history-file-name "~/.influx_history")

(defvar influxdb-mode-hook nil)

(define-derived-mode influxdb-mode comint-mode "InfluxDB"
  (run-hooks 'influxdb-mode-hook))

(defun influx ()
  "InfluxDB cli"
  (interactive)
  (let ((default-directory "~"))
    (unless (comint-check-proc "*influx*")
      (make-comint "influx" influxdb-cli nil "-host" influxdb-host "-port" (format "%s" influxdb-port) "-database" influxdb-database "-precision" influxdb-precision))
    (switch-to-buffer "*influx*")
    (setq comint-input-ring (make-ring comint-input-ring-size))
    (setq comint-input-ring-file-name influxdb-history-file-name)
    (comint-read-input-ring 'silent)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'influx-process-kill-buffer-sentinel)
    (influxdb-mode)))

(defun influx-process-kill-buffer-sentinel (process state)
  (message "influx(%s): %s" (buffer-name) state)
  (kill-buffer (current-buffer)))

(provide 'influxdb-mode)
