FROM alpine

WORKDIR /code

ENV PATH $PATH:/usr/local/bin
ENV apk_packages bash sbcl tini
ENV apk_repo http://dl-3.alpinelinux.org/alpine/edge/testing/ 

RUN apk --update --repository ${apk_repo} add ${apk_packages}

COPY . .

ENTRYPOINT ["tini", "--"]

CMD ["sbcl", "--load", "src/class.lisp"]

