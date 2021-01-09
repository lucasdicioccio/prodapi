#!/bin/bash
baseurl="http://localhost:8000"
cookies="cookiejar"

function addPing() {
	local target="$1"
	echo "PINGING ${target}"
	curl -XPOST \
		-b "${cookies}" \
		-d "\"${target}\"" \
		-H 'Content-Type: application/json' \
		"${baseurl}/monitors/ping"
	}

function delPing() {
	local target="$1"
	echo "REMOVE PINGING ${target}"
	curl -XDELETE \
		-b "${cookies}" \
		-d "\"${target}\"" \
		-H 'Content-Type: application/json' \
		"${baseurl}/monitors/ping"
	}

function readPing() {
	local target="$1"
	echo "READ PING ${target}"
	curl -XGET \
		-b "${cookies}" \
		-H 'Content-Type: application/json' \
		"${baseurl}/monitors/ping/latest?target=${target}"
	}

echo "ADDING"
addPing "8.8.8.8"
addPing "8.8.4.4"

sleep 10
readPing "8.8.8.8"
readPing "8.8.4.4"

sleep 10
echo "DELETING"
delPing "8.8.8.8"
delPing "8.8.4.4"
