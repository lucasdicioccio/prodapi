
baseurl="http://localhost:8000"
cookies="cookiejar"

echo "REGISTERING"
curl -XPOST \
	-H 'Content-Type: application/json' \
	-d '{"email": "foo@example.com", "plain": "fake-password"}' \
	"${baseurl}/user-auth/registration"
echo

echo "LOGIN-IN"
curl -c "${cookies}" \
	-XPOST \
	-H 'Content-Type: application/json' \
	-d '{"email": "foo@example.com", "plain": "fake-password"}' \
	"${baseurl}/user-auth/login"
echo

echo "CHECKING LOGGED-STATUS"
curl -XGET \
	-b "${cookies}" \
	-H 'Content-Type: application/json' \
	"${baseurl}/user-auth/whoami"
echo

echo "CHECKING ECHO INFO"
curl -XGET \
	-b "${cookies}" \
	-H 'Content-Type: application/json' \
	"${baseurl}/user-auth/echo-cookie"
echo
