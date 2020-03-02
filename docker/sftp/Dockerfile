FROM atmoz/sftp:alpine

COPY data/keys/ /etc/ssh/

HEALTHCHECK --interval=10s --timeout=10s --retries=10 CMD netstat -an | grep "0.0.0.0:22" -q
