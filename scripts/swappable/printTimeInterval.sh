#!/bin/bash
set -e
if [[ $# -eq 0 ]] ; then
    echo 'No Time Supplied'
    exit 1
fi
timeInterval=$((${1}* 60 * 1000))

echo "A" ${1} "Min Interval is" ${timeInterval}

echo "Start:" `expr $(echo $(date +%s%3N)) + $(echo 0)`

echo "End:" `expr $(echo $(date +%s%3N)) + $(echo ${timeInterval})`