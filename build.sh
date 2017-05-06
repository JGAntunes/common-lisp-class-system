#!/bin/bash

docker build --rm --tag pav-lisp .

if [ "$RUN" != "" ]; then
  docker run -it --rm pav-lisp
fi 
