# Judith Tonhauser, May 2018, Bhili-Kandeshi prosody project

# This script extracts the f0 values of the L and the H annotated on tier 4.
# It also provides the time of the L and H, the first and last time of the expression that L and H
# are associated with.

form Get f0 of L and H on tier 4
	sentence Directory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-utterances/
	word Base_file_name 
	comment Output file
	text textfile /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-LHvalues.csv
	comment How frequently should an f0 value be extracted?
	positive time_step_(s) 0.01
endform

#Read all files in a folder
Create Strings as file list... wavlist 'directory$'/'base_file_name$'*.wav
Create Strings as file list... gridlist 'directory$'/'base_file_name$'*.TextGrid
n = Get number of strings

fileappend 'textfile$' File,labelPoint,label,f0,time,begTimeLabel,endTimeLabel 'newline$'

for i to n
clearinfo

# Get the list of files in directory
	select Strings wavlist
	filename$ = Get string... i
	Read from file... 'directory$'/'filename$'
	soundname$ = selected$ ("Sound")

# Set appropriate f0 values for extraction
minimum_pitch = 0
maximum_pitch = 0

	if startsWith (soundname$, "GUJ-P1-") or startsWith (soundname$, "GUJ-P12-") or startsWith (soundname$, "GUJ-P7-") 
		minimum_pitch = 50
		maximum_pitch = 300
	elsif startsWith (soundname$, "GUJ-P11-") or startsWith (soundname$, "GUJ-P6-") or startsWith (soundname$, "GUJ-P3-") or startsWith (soundname$, "GUJ-P9-")
		minimum_pitch = 50
		maximum_pitch = 400
	elsif startsWith (soundname$, "GUJ-P10-") or startsWith (soundname$, "GUJ-P2-") or startsWith (soundname$, "GUJ-P4-") or startsWith (soundname$, "GUJ-P5-") or startsWith (soundname$, "GUJ-P8-")
		minimum_pitch = 75
		maximum_pitch = 400
	endif


# Extract pitch tiers
	To Pitch... time_step minimum_pitch maximum_pitch
	output$ = "'soundname$'.Pitch"

# We now read grid files and extract all intervals in them
	select Strings gridlist
	gridname$ = Get string... i
	Read from file... 'directory$'/'gridname$'
	int=Get number of intervals... 1

# Get beginning of utterance (2nd interval) and end of utterance (n-1 interval)
	uttStart = Get starting point... 1	2
	lastWord = 'int'-1
	uttEnd = Get end point... 1 lastWord

# Get f0min and f0max of utterance
	select Pitch 'soundname$'
	uttf0min = Get minimum... uttStart uttEnd Hertz Parabolic
	uttf0max = Get maximum... uttStart uttEnd Hertz Parabolic
	
# extract f0 values of L and H on tier 4 and label of L and H

	select TextGrid 'soundname$'

	# find the points on tier 4
	points = Get number of points... 4
	#writeInfo: points

	for j from 1 to points

		# first and third points is are on subject and predicate
		if j = 1
			select TextGrid 'soundname$'
			thisPoint$ = Get label of point... 4 j
			timePoint = Get time of point... 4 j
			wordInt = Get interval at time... 1 timePoint
			word$ = Get label of interval... 1 wordInt
			#writeInfo: wordInt
			start = Get starting point... 1 wordInt
			#appendInfo: start
			end = Get end point... 1 wordInt

			select Pitch 'soundname$'
			f0 = Get value at time... timePoint Hertz Linear
			writeInfo: f0

			fileappend 'textfile$' 'soundname$','thisPoint$','word$','f0','timePoint','start','end''newline$'
	
		# second and fourth points are H on subject and predicate (extract label of word 1ms before time of point)
		elsif j = 2
			select TextGrid 'soundname$'
			thisPoint$ = Get label of point... 4 j
			timePoint = Get time of point... 4 j
			timePointN = timePoint - 0.01
			wordInt = Get interval at time... 1 timePointN
			word$ = Get label of interval... 1 wordInt
			#writeInfo: wordInt
			start = Get starting point... 1 wordInt
			#appendInfo: start
			end = Get end point... 1 wordInt

			select Pitch 'soundname$'
			f0 = Get value at time... timePoint Hertz Linear
			writeInfo: f0

			fileappend 'textfile$' 'soundname$','thisPoint$','word$','f0','timePoint','start','end''newline$'
		
			# first and third points is are on subject and predicate
		elsif j = 3
			select TextGrid 'soundname$'
			thisPoint$ = Get label of point... 4 j
			timePoint = Get time of point... 4 j
			wordInt = Get interval at time... 1 timePoint
			word$ = Get label of interval... 1 wordInt
			#writeInfo: wordInt
			start = Get starting point... 1 wordInt
			#appendInfo: start
			end = Get end point... 1 wordInt

			select Pitch 'soundname$'
			f0 = Get value at time... timePoint Hertz Linear
			writeInfo: f0

			fileappend 'textfile$' 'soundname$','thisPoint$','word$','f0','timePoint','start','end''newline$'
	
		# second and fourth points are H on subject and predicate (extract label of word 1ms before time of point)
		elsif j = 4
			select TextGrid 'soundname$'
			thisPoint$ = Get label of point... 4 j
			timePoint = Get time of point... 4 j
			timePointN = timePoint - 0.01
			wordInt = Get interval at time... 1 timePointN
			word$ = Get label of interval... 1 wordInt
			#writeInfo: wordInt
			start = Get starting point... 1 wordInt
			#appendInfo: start
			end = Get end point... 1 wordInt

			select Pitch 'soundname$'
			f0 = Get value at time... timePoint Hertz Linear
			writeInfo: f0

			fileappend 'textfile$' 'soundname$','thisPoint$','word$','f0','timePoint','start','end''newline$'
		endif

	endfor
endfor

# clean up
select all
Remove