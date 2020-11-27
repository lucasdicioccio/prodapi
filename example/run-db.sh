#!/usr/bin/env bash
createuser prodapi
dropdb prodapi_example
createdb -O prodapi prodapi_example
psql -c "ALTER ROLE prodapi PASSWORD 'prodapi';"
psql -d prodapi_example -f "prodapi/scripts/0001-add-crypto.sql"
psql -d prodapi_example -f "prodapi/scripts/0002-add-uuid.sql"
psql -d prodapi_example -f "prodapi/scripts/0003-add-userauth.sql"
psql -d prodapi_example -c "GRANT ALL ON TABLE identities TO prodapi"
psql -d prodapi_example -c "GRANT ALL ON SEQUENCE identities_id_seq TO prodapi"
psql -d prodapi_example -c "GRANT ALL ON TABLE passwords TO prodapi"
psql -d prodapi_example -c "GRANT ALL ON TABLE password_lost_request TO prodapi"
psql -d prodapi_example -c "GRANT ALL ON SEQUENCE password_lost_request_id_seq TO prodapi"
