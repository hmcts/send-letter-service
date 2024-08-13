# SFTP Local

This should be run as a part of the dev-env (common-dev-env-bsbp), but if you want to run locally, you simply
need to do the following:

```shell
docker-compose down send-letter-sftp
docker-compose up --build -d send-letter-sftp
```

# To run on FileZilla
Create a new site, set the username as `mosh` and the type as `keyfile`.

Set the host to `localhost` and port to `2222`.

Make sure to select `id_dev_rsa.txt` when searching for the key file to use.
