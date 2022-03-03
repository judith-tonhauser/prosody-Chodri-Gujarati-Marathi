# Chodri
# This script extracts the f0 values of the subject and predicate of Chodri utterances, every 10ms
# It is sensitive to the f0 min and max of the talkers
# modified by Judith Tonhauser, May 2018, Bhili-Kandeshi prosody project

form Get F0 Min-Max-Min
	sentence Directory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Chodri-utterances/
	word Base_file_name 
	comment Output file
	text textfile /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Chodri-pitch-by-time.csv
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

	if startsWith (soundname$, "CHO-P11-") or startsWith (soundname$, "CHO-P5-") or startsWith (soundname$, "CHO-P1-") 
		minimum_pitch = 50
		maximum_pitch = 350
	elsif startsWith (soundname$, "CHO-P10-") or startsWith (soundname$, "CHO-P3-") or startsWith (soundname$, "CHO-P12-") or startsWith (soundname$, "CHO-P6-") or startsWith (soundname$, "CHO-P8-") or startsWith (soundname$, "CHO-P9-")
		minimum_pitch = 75
		maximum_pitch = 400
	elsif startsWith (soundname$, "CHO-P7-") or startsWith (soundname$, "CHO-P4-") 
		minimum_pitch = 100
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
	#writeInfoLine: gridname$

# Extract f0 values in subjects and predicates 
for k from 1 to 'int'
	select TextGrid 'soundname$'
	label$ = Get label of interval... 1 'k'
	if (label$ = "ananyaa" and startsWith (soundname$, "CHO-P8-target6")) or (label$ = "ananyaa" and startsWith (soundname$, "CHO-P8-target4")) or (label$ = "laavanya" and startsWith (soundname$, "CHO-P10-target2"))
		startTime = Get starting point... 1 'k'
		#writeInfoLine: startTime
		endTime = Get end point... 1 'k'
		select Pitch 'soundname$'

		first_frame = 1
		writeInfoLine: first_frame
		last_frame = Get frame from time... 'endTime'
		appendInfoLine: last_frame

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

	elsif (label$ = "aaraam" or label$ = "ananyaa" or label$ = "kaam" or label$ = "laavanya" or label$ = "kheDvaanu" or label$ = "nedvaanu" or label$ = "aaraam" or label$ = "kasrat")
		startTime = Get starting point... 1 'k'
		#writeInfoLine: startTime
		endTime = Get end point... 1 'k'
		select Pitch 'soundname$'

		first_frame = Get frame from time... 'startTime'
		#writeInfoLine: first_frame
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