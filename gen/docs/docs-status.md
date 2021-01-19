Up to date
## GET /status

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/html;charset=utf-8`
    - `application/json;charset=utf-8`
    - `application/json`

- a status page recapitulates liveness, healthiness, and has extras (`text/html;charset=utf-8`):

```html
<html><head><title>status page</title><link href="status.css" type="text/css" rel="stylesheet"><script async type="text/javascript" src="metrics.js"></script></head><body><section><h1>identification</h1><p>df557b3a-3ca3-4725-b14d-3596fbbc2fe8</p></section><section><h1>general status</h1><p><a href="/health/alive">alive</a></p><p><a href="/health/ready">ready</a></p><form action="/health/drain" method="post"><input value="drain me" type="submit"></form></section><section><h1>app status</h1><section><h1>example tunable status</h1><p>you can tune your status page</p></section></section></body></html>
```

- a status page recapitulates liveness, healthiness, and has extras (`application/json;charset=utf-8`, `application/json`):

```javascript
{"status":{"exampleStatus":"example tunable status"},"readiness":{"tag":"Ready"},"id":"df557b3a-3ca3-4725-b14d-3596fbbc2fe8","liveness":"alive"}
```


