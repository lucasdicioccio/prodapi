Up to date
## POST /user-auth/clean-cookie

### deletes the cookie


### Response:

- Status code 200
- Headers: [("Set-Cookie","login-jwt=login-jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqd3QtYXBwIiwidXNlci1pZCI6NH0.v88HCeDuNsk83umM291-2JT6kgnHYSczld9oU3TnI0s; Path=/; SameSite=Strict; HttpOnly; Path=/; SameSite=Strict; HttpOnly")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- the unit type, representing an absence of return value (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## GET /user-auth/echo-cookie

### echo cookie claims after validating it


### Headers:

- This endpoint is sensitive to the value of the **Cookie** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- some claims set with issuer etc. (`application/json;charset=utf-8`, `application/json`):

```javascript
{"iss":"issuer","sub":"..."}
```

## GET /user-auth/hello

### Hello World with Auth Protect


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- some arbitrary text (`application/json;charset=utf-8`, `application/json`):

```javascript
"lorem ipsum"
```

## POST /user-auth/login

### login as a user


### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json;charset=utf-8`
    - `application/json`

- temptative login (`application/x-www-form-urlencoded`):

```
email=foo%40example.com&plain=secret
```

- temptative login (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@example.com","plain":"secret"}
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","login-jwt=login-jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqd3QtYXBwIiwidXNlci1pZCI6NH0.v88HCeDuNsk83umM291-2JT6kgnHYSczld9oU3TnI0s; Path=/; SameSite=Strict; HttpOnly; Path=/; SameSite=Strict; HttpOnly")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- sucessful login with user id 1234 (`application/json;charset=utf-8`, `application/json`):

```javascript
{"contents":{"info":"the robot","userId":1234},"tag":"LoginSuccess"}
```

- failed login (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"LoginFailed"}
```

## POST /user-auth/recovery/apply

### overwrites the password


### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json;charset=utf-8`
    - `application/json`

- apply a token received out of bound (`application/x-www-form-urlencoded`):

```
email=foo%40example.com&plain=new-password&token=secret%20token%20received%20out%20of%20bound
```

- apply a token received out of bound (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@example.com","plain":"new-password","token":"secret token received out of bound"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- successfully changed pass (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"RecoverySuccess"}
```

- failed to change pass (`application/json;charset=utf-8`, `application/json`):

```javascript
{"contents":"some reason","tag":"RecoveryFailed"}
```

## POST /user-auth/recovery/request

### request an out-of-band password recovery


### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json;charset=utf-8`
    - `application/json`

- recovery request (`application/x-www-form-urlencoded`):

```
email=foo%40example.com
```

- recovery request (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@example.com"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- recovery request notification valid for 60minutes (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@example.com","minutes":60,"token":"random-bytes"}
```

## POST /user-auth/registration

### register a new user


### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json;charset=utf-8`
    - `application/json`

- some registration (`application/x-www-form-urlencoded`):

```
email=foo%40example.com&plain=my%20desired%20pass
```

- some registration (`application/json;charset=utf-8`, `application/json`):

```javascript
{"email":"foo@example.com","plain":"my desired pass"}
```

### Response:

- Status code 200
- Headers: [("Set-Cookie","login-jwt=login-jwt=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJqd3QtYXBwIiwidXNlci1pZCI6NH0.v88HCeDuNsk83umM291-2JT6kgnHYSczld9oU3TnI0s; Path=/; SameSite=Strict; HttpOnly; Path=/; SameSite=Strict; HttpOnly")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- successful registration with user id 1234 (`application/json;charset=utf-8`, `application/json`):

```javascript
{"contents":{"info":"the robot","userId":1234},"tag":"RegisterSuccess"}
```

- failed registration (`application/json;charset=utf-8`, `application/json`):

```javascript
{"tag":"RegisterFailure"}
```

## GET /user-auth/whoami

### prints user identities for a cookie


### Headers:

- This endpoint is sensitive to the value of the **Cookie** HTTP header.

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- i am a robot (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"email":"a robot","info":"with some metal head"}]
```

- i am a robot, i am a robot (`application/json;charset=utf-8`):

```javascript
[{"email":"a robot","info":"with some metal head"},{"email":"a robot","info":"with some metal head"}]
```


