#!/bin/bash

grep -e 'http:\/\/www.nfl.com\/teams\/[a-z]*\/profile?' NFL\ Teams.htm > nfllinks.txt
cat nfllinks.txt | gawk '{FS="=\""}{ print $3}' | sed 's/onclick//g' > nflink2.txt
nflteams=($(cat nfllinks2.txt | gawk '{FS="/"}{ print $5","substr($6,14,3) }' | uniq))

for i in "${nflteams[@]}"; do echo "http://www.nfl.com/teams/"${i%????}"/roster?team="${i: -3}; done




