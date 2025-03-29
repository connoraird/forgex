#!/bin/bash
fpm clean
fpm build --profile debug
find build | grep .mod | xargs -i cp {} mod/.
