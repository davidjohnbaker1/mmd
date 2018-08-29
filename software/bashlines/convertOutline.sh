sed 's/\ \ \+/\#\#/g' $1 | # Level 2 Header 
sed 's/\ \ \ \+/\#\#\#/g' $1 | # Level 3 Header 	
sed 's/\ \ \ \ \+/\#\#\#\#/g' $1| # Level 4 Header
sed 's/\ \ \ \ \ \+/\#\#\#\#\#/g' $1 # Level 4 Header
