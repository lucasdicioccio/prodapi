Up to date
## POST /reports

### receives and acknowledge some reports


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an example of stack-trace reporting (`application/json;charset=utf-8`, `application/json`):

```javascript
{"es":[{"stackTrace":["err toto.js at 236: undefined is not a function"]}],"t":1611183428,"b":0}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an example integer (`application/json;charset=utf-8`, `application/json`):

```javascript
42
```


