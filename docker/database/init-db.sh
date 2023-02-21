#!/usr/bin/env bash

set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE USER letterservice;
    CREATE DATABASE letterservice
        WITH OWNER = letterservice
        ENCODING ='UTF-8'
        CONNECTION LIMIT = -1;
EOSQL

psql -v ON_ERROR_STOP=1 --dbname=letterservice --username "$POSTGRES_USER" <<-EOSQL
    CREATE SCHEMA letterservice AUTHORIZATION letterservice;
EOSQL

psql -v ON_ERROR_STOP=1 --dbname=letterservice --username "$POSTGRES_USER" <<-EOSQL
    CREATE EXTENSION lo;
EOSQL
