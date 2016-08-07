#!/usr/bin/env bash

echo '('

cat written-to-be-spoken.txt books.txt laws.txt school_essays.txt  | rstrip | grep . | cut -f1 | grep ... | lower | python syllables..py | grep '[aeioyuáóýúíé]' |grep -v '[.!,]' | sort | uniq -c | sort -nr | head -500 | lstrip | cuts -f2

echo ')'
