# Marathi
# This script extracts the f0 values of the subject and predicate of Marathi utterances, every 10ms
# It is sensitive to the f0 min and max of the talkers
# modified by Judith Tonhauser, May 2018, Bhili-Kandeshi prosody project

form Get F0 Min-Max-Min
	sentence Directory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Marathi-utterances/
	word Base_file_name 
	comment Output file
	text textfile /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Marathi-pitch-by-time.csv
	comment How frequently should an f0 value be extracted?
	positive Time_step_(s) 0.01
endform

#Read all files in a folder
Create Strings as file list... wavlist 'directory$'/'base_file_name$'*.wav
Create Strings as file list... gridlist 'directory$'/'base_file_name$'*.TextGrid
n = Get number of strings
#writeInfoLine: n

fileappend 'textfile$' File,label,time,f0 'newline$'

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

	if startsWith (soundname$, "MAR-P4-") or startsWith (soundname$, "MAR-P5-") or startsWith (soundname$, "MAR-P7-") or startsWith (soundname$, "MAR-P8-") or startsWith (soundname$, "MAR-P9-")
		minimum_pitch = 50
		maximum_pitch = 350
	elsif startsWith (soundname$, "MAR-P1-") or startsWith (soundname$, "MAR-P6-") or startsWith (soundname$, "MAR-P11-") or startsWith (soundname$, "MAR-P3-")
		minimum_pitch = 75
		maximum_pitch = 400
	elsif startsWith (soundname$, "MAR-P10-") or startsWith (soundname$, "MAR-P2-")
		minimum_pitch = 75
		maximum_pitch = 500
	endif

# Extract pitch tiers
	To Pitch... time_step minimum_pitch maximum_pitch
	output$ = "'soundname$'.Pitch"

# Read grid files and extract all intervals in them
	select Strings gridlist
	gridname$ = Get string... i
	Read from file... 'directory$'/'gridname$'
	int=Get number of intervals... 1
	writeInfoLine: gridname$

# Extract f0 values in subject and predicate 
for k from 1 to 'int'
	select TextGrid 'soundname$'
	label$ = Get label of interval... 1 'k'
	if (label$ = "aaraam" or label$ = "ananyaa" or label$ = "laavanya" or label$ = "laavanyaa" or label$ = "naangarNi" or label$ = "nindaNi" or label$ = "nindanNi" or label$ = "vyaayaam")
		startTime = Get starting point... 1 'k'
		#writeInfoLine: startTime
		endTime = Get end point... 1 'k'
		select Pitch 'soundname$'

		first_frame = Get frame from time... 'startTime'
		last_frame = Get frame from time... 'endTime'

		for j from 'first_frame' to 'last_frame'+1

			t_frame = Get time from frame... j
			#writeInfoLine: t_frame
			f0 = Get value at time... 't_frame' Hertz Linear
			if f0 <> undefined
				fileappend 'textfile$' 'soundname$','label$','t_frame','f0' 'newline$'
			elsif f0 = undefined
				fileappend 'textfile$' 'soundname$','label$','t_frame',NA'newline$'
			endif

		#writeInfoLine: numberOfFrames
	
		endfor
	endif
endfor
	
endfor

# clean up
select all
Remove