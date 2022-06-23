#! /bin/bash

cat << DELIMITER > whitout_minus.txt
	une phrase
deux phrase
	trois phrase
		quatre phrase
DELIMITER


cat <<- DELIMITER > minus.txt
une phrase
	deux phrase
trois phrase
		quatre phrase
DELIMITER