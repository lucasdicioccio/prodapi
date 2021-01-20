Up to date
## GET /metrics

### Prometheus metrics


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/plain;charset=utf-8`

- some prometheus example (`text/plain;charset=utf-8`):

```
# HELP prodapi_dns_discovery_results 
# TYPE prodapi_dns_discovery_results gauge
prodapi_dns_discovery_results{m="dig",type="A",host="dicioccio.fr"} 3.0
# HELP prodapi_dns_discoveries 
# TYPE prodapi_dns_discoveries counter
prodapi_dns_discoveries{m="dig",type="A",host="dicioccio.fr"} 257.0
# HELP prodapi_watchdog_filetouch 
# TYPE prodapi_watchdog_filetouch counter
prodapi_watchdog_filetouch{status="success",path="./example-prodapi-watchdog"} 516.0
# HELP userauth_recovery_applied number of recovery-requests applied
# TYPE userauth_recovery_applied counter
userauth_recovery_applied{status="ok"} 1.0
userauth_recovery_applied{status="requested"} 2.0
# HELP userauth_recovery_requests number of recovery-requests received
# TYPE userauth_recovery_requests counter
userauth_recovery_requests{status="ok"} 1.0
userauth_recovery_requests{status="requested"} 1.0
# HELP userauth_registrations number of registrations
# TYPE userauth_registrations counter
userauth_registrations{status="ok"} 1.0
userauth_registrations{status="requested"} 2.0
# HELP watchdog_hello continuously working
# TYPE watchdog_hello counter
watchdog_hello{status="success"} 5147.0

```


