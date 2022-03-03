# Judith Tonhauser, March 2018, for Bhili-Kandeshi prosody project
#
# This script loops through the text grids in a directory and 
# extracts the durations of the words on tier 1 and syllables on tier 2.
# The durations are saved in a CSV file, each line containing the file name and the label of the 
# word or syllable. 

form GetToBi
	sentence wavdirectory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-utterances/
	sentence output  /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-durations.csv
	#boolean cleanup 1
	boolean print_header 1
endform

textgriddirectory$ = wavdirectory$

list = Create Strings as file list... list 'wavdirectory$'*.wav

if print_header = 1
	fileappend 'output$' File,totalDuration,label,duration 'newline$'
endif

numberOfFiles = Get number of strings

# loop through each textgrid
for file to numberOfFiles
	select Strings list
	filename$ = Get string... file
	basenamelength = length(filename$) - 4
	textgrid$ = textgriddirectory$ + left$(filename$, basenamelength) + ".TextGrid"
	textgrid = Read from file... 'textgrid$'

# utterance duration (from beginning of 2nd interval to end of last labeled interval on tier 1)
	select 'textgrid'
	#fileappend 'output$' 'filename$',
	total_duration = 0
	numberOfIntervals = Get number of intervals... 1
	numberOfSyls = Get number of intervals... 2
	#writeInfoLine: numberOfIntervals
	lastWord = numberOfIntervals-1
	#writeInfoLine: lastWord
	start = Get starting point... 1 2
	#writeInfoLine: start
	end = Get end point... 1 lastWord
	#writeInfoLine: end
	totalDuration = end - start
	#writeInfoLine: totalDuration
	#fileappend 'output$' 'totalDuration' 

# word durations (for all labeled intervals on tier 1) 

	# loop through the words in word tier (tier 1) and extract labels and durations of words
	for i from 1 to numberOfIntervals
		label$ = Get label of interval... 1 i
		#writeInfoLine: label$
		# The next line will make sure that intervals with empty labels are not included:
		if label$ <> ""
			#writeInfoLine: label$
			start = Get starting point... 1 i	
			end = Get end point... 1 i
			duration = end - start
			fileappend 'output$' 'filename$','totalDuration','label$','duration' 'newline$'
		endif
	endfor

# syllable durations (for all labeled intervals on tier 2)

	# loop through the syllables in syl tier (tier 2) and extract labels and durations of syls
	for i from 1 to numberOfSyls
		label$ = Get label of interval... 2 i
		#writeInfoLine: label$
		# The next line will make sure that kaam (which is a word) and intervals with empty labels are not included:
		if label$ <> "" & label$ <> "kaam"
			#writeInfoLine: label$
			start = Get starting point... 2 i	
			end = Get end point... 2 i
			duration = end - start
			fileappend 'output$' 'filename$','totalDuration','label$','duration' 'newline$'
		endif
	endfor
Remove
endfor

