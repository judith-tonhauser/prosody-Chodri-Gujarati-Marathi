# Judith Tonhauser, March 2018, Bhili-Kandeshi prosody project

# This Praat assumes that there is a rise in each of the first two words of the utterances
# The script will give the initial f0min and the following f0max within the word
# This script analayzes all the files in a specifid folder.

form Get F0 Min-Max-Min
	sentence Directory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Marathi-utterances/
	word Base_file_name 
	comment Output file
	text textfile /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Marathi-f0values.csv
endform

#Read all files in a folder
Create Strings as file list... wavlist 'directory$'/'base_file_name$'*.wav
Create Strings as file list... gridlist 'directory$'/'base_file_name$'*.TextGrid
n = Get number of strings

fileappend 'textfile$' File,label,startTime,endTime,f0min,f0max,minTime,maxTime,uttf0min,uttf0max 'newline$'

for i to n
clearinfo
#We first extract pitch tiers
	select Strings wavlist
	filename$ = Get string... i
	Read from file... 'directory$'/'filename$'
	soundname$ = selected$ ("Sound")
	To Pitch... 0.01 75 400
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
	
# extracts f0min and following f0max within same word
for k from 1 to 'int'
	select TextGrid 'soundname$'
	label$ = Get label of interval... 1 'k'
	if (label$ = "ananyaa" or label$ = "laavanya" or label$ = "laavanyaa" or label$ = "aaraam" or label$ = "nindaNi" or label$ = "vyaayaam" or label$ = "naangarNi")
		startTime = Get starting point... 1 'k'
		endTime = Get end point... 1 'k'
		select Pitch 'soundname$'
		f0min = Get minimum... startTime endTime Hertz Parabolic
		minTime = Get time of minimum... startTime endTime Hertz Parabolic
		if minTime = endTime
			maxTime = minTime
		else 
			f0max = Get maximum... minTime endTime Hertz Parabolic
			maxTime = Get time of maximum... minTime endTime Hertz Parabolic
		endif
		fileappend 'textfile$' 'soundname$','label$','startTime','endTime','f0min','f0max','minTime','maxTime','uttf0min','uttf0max' 'newline$'
	endif
endfor
	
endfor

# clean up
select all
Remove