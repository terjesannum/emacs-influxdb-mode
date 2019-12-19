# emacs-influxdb-mode

Comint mode for InfluxDB CLI

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-influxdb-mode")
(require 'influxdb-mode)
```

Start influxdb cli with `M-x influx`

### Configuration

Database to connect to is defined in the variables `influxdb-host`, `influxdb-port` and `influxdb-database`,
set those or use environment variables `INFLUX_HOST`, `INFLUX_HOST` and `INFLUX_DATABASE`.
