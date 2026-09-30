## POST /reports

### receives and acknowledge some reports


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an example of stack-trace reporting (`application/json;charset=utf-8`, `application/json`):

```json
{"b":0,"es":[{"stackTrace":["err toto.js at 236: undefined is not a function"]}],"t":1611183428}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- an example integer (`application/json;charset=utf-8`, `application/json`):

```json
42
```


