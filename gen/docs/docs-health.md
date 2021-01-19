Up to date
## GET /health/alive

### Health liveness probe.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an application is alive if it can returns some string, hence there is a single value possible (`application/json;charset=utf-8`, `application/json`):

```javascript
"alive"
```

## POST /health/drain

### Set a specific 'drained' condition.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- ready to serve requests (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"Ready"}
```

- should not be serving requests for _some reason_ (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"Ill","contents":["some reason"]}
```

## GET /health/ready

### Health readiness probe.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- ready to serve requests (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"Ready"}
```

- should not be serving requests for _some reason_ (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"Ill","contents":["some reason"]}
```


