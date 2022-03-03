##############################
#
#For all TextGrid files in a directory, extract label of syllable associated with L and H on tier 4
# when the H is at the end of the subject, it is sometimes mis-associated with the predicate
# to prevent that, the label of the syllable and word of the H of the subject is extracted at 10ms before
# the time of the H
# Saves data in a .csv file
# Judith Tonhauser, May 2018 for Bhili-Kandeshi prosody project
####################################

form GetToBi
	sentence wavdirectory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-utterances/
	sentence output  /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-LH-syllables.csv
endform

textgriddirectory$ = wavdirectory$

list = Create Strings as file list... list 'wavdirectory$'*.wav

fileappend 'output$' File,Tone,Time,Word,Syllable 'newline$'

numberOfFiles = Get number of strings

for file to numberOfFiles
	select Strings list
	filename$ = Get string... file
	basenamelength = length(filename$) - 4
	textgrid$ = textgriddirectory$ + left$(filename$, basenamelength) + ".TextGrid"
	textgrid = Read from file... 'textgrid$'
	
	select 'textgrid'

	# find the points on tier 4
	points = Get number of points... 4

	for j from 1 to points

	# first point is L on subject
	if j = 1
		thisPoint$ = Get label of point... 4 j
		timePoint = Get time of point... 4 j
		wordInt = Get interval at time... 1 timePoint
		word$ = Get label of interval... 1 wordInt
		syllableInt = Get interval at time... 2 timePoint
		syllable$ = Get label of interval... 2 syllableInt
		fileappend 'output$' 'filename$','thisPoint$','timePoint','word$','syllable$''newline$'
	
	# second point is H on subject
	elsif j = 2
		thisPoint$ = Get label of point... 4 j
		timePoint = Get time of point... 4 j
		timePointN = timePoint - 0.01
		wordInt = Get interval at time... 1 timePointN
		word$ = Get label of interval... 1 wordInt
		syllableInt = Get interval at time... 2 timePointN
		syllable$ = Get label of interval... 2 syllableInt
		fileappend 'output$' 'filename$','thisPoint$','timePoint','word$','syllable$''newline$'


	# third point is L on predicate
	elsif j = 3
		thisPoint$ = Get label of point... 4 j
		timePoint = Get time of point... 4 j
		wordInt = Get interval at time... 1 timePoint
		word$ = Get label of interval... 1 wordInt
		syllableInt = Get interval at time... 2 timePoint
		syllable$ = Get label of interval... 2 syllableInt
		fileappend 'output$' 'filename$','thisPoint$','timePoint','word$','syllable$''newline$'


	# third point is H on predicate
	elsif j = 4
		thisPoint$ = Get label of point... 4 j
		timePoint = Get time of point... 4 j
		timePointN = timePoint - 0.01
		wordInt = Get interval at time... 1 timePointN
		word$ = Get label of interval... 1 wordInt
		syllableInt = Get interval at time... 2 timePointN
		syllable$ = Get label of interval... 2 syllableInt
		fileappend 'output$' 'filename$','thisPoint$','timePoint','word$','syllable$''newline$'
	endif
	endfor
Remove
endfor

