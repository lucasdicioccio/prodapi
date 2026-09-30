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
<html><head><title>status page</title><link rel="stylesheet" type="text/css" href="status.css"></head><body><section><h1>identification</h1><p>468456e9-0945-477e-b756-be42af451eb3</p></section><section><h1>general status</h1><p><a href="/health/alive">alive</a></p><p><a href="/health/ready">ready</a></p><form action="/health/drain" method="post"><input type="submit" value="drain me"></form></section><section><h1>app status</h1><section><h1>example tunable status</h1><p>note that you can tune your status page</p></section></section></body></html>
```

- a status page recapitulates liveness, healthiness, and has extras (`application/json;charset=utf-8`, `application/json`):

```json
{"id":"468456e9-0945-477e-b756-be42af451eb3","liveness":"alive","readiness":{"tag":"Ready"},"status":{"exampleStatus":"example tunable status"}}
```


