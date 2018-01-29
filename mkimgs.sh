#!/bin/sh

set -e

width=1024
height=768

if [ $# -ne 1 ]; then
    echo "Usage: $0 <pdf>" >&2
    exit 1
fi

pdftoppm -scale-to-x $width -scale-to-y $height -png "$1" imgs/
