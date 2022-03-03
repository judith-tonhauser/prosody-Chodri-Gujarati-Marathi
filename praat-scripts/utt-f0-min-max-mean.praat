# Judith Tonhauser, June 2018, Bhili-Kandeshi prosody project

# This Praat assumes extracts, for each utterance, the f0min, f0mean and f0max
# The output is used to determine an appropriate f0 range to extract f0 values for
# the GAMM analysis.

form Get F0 Min-Max-Min
	sentence Directory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-utterances/
	word Base_file_name 
	comment Output file
	text textfile /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-f0values.csv
endform

#Read all files in a folder
Create Strings as file list... wavlist 'directory$'/'base_file_name$'*.wav
Create Strings as file list... gridlist 'directory$'/'base_file_name$'*.TextGrid
n = Get number of strings

fileappend 'textfile$' File,uttf0min,uttf0max,uttf0mean 'newline$'

for i to n
clearinfo
#We first extract pitch tiers
	select Strings wavlist
	filename$ = Get string... i
	Read from file... 'directory$'/'filename$'
	soundname$ = selected$ ("Sound")
	To Pitch... 0.01 75 550
	output$ = "'soundname$'.Pitch"
	# Write to binary file... 'output$'

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
	uttf0mean = Get mean... uttStart uttEnd Hertz
	
	fileappend 'textfile$' 'soundname$','uttf0min','uttf0max','uttf0mean' 'newline$'
	endif
endfor

# clean up
select all
Remove