#################################################################
## add-tier4-LH.praat
## 
## For the TextGrid of all .wav files in a directory, 
##	add tier 4 and annotate L and H on that tier
##
## Adapted by Judith Tonhauser May 2018 for prosody of Khandeshi-Bhili languages project
#################################################################

form Input 
	#check box if TextGrids already exist, otherwise uncheck box
	sentence DirName /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Chodri-utterances/next-five/
endform

Create Strings as file list... fileList 'dirName$'*.wav
nFiles = Get number of strings
for i to nFiles
	# get sound filename
	select Strings fileList
	fileName$ = Get string... i
	Read from file... 'dirName$''fileName$'
	name$ = selected$("Sound")

	# create Pitch object
	To Pitch... 0.01 75 500
	output$ = "'name$'.Pitch"

	# if tier 4 already exists, remove it
	Read from file... 'dirName$''name$'.TextGrid
	select TextGrid 'name$'
	tiers = Get number of tiers
	if tiers = 4
		Remove tier... 4
	endif
			
	#add tier 4
	Insert point tier... 4 LH

	# get all intervals on tier 1
	int=Get number of intervals... 1
	#writeInfoLine: int

	# if subject or predicate, annotate L and H
	for k from 1 to 'int'
	writeInfoLine: k
		label$ = Get label of interval... 1 'k'
		# subjects
		if (label$ = "ananyaa" or label$ = "laavanya" or label$ = "laavanyaa")
			startTime = Get starting point... 1 'k'
			endTime = Get end point... 1 'k'	
			#writeInfoLine: label$
			select Pitch 'name$'
			#writeInfoLine: name$
			minTime = Get time of minimum... startTime endTime Hertz Parabolic
			maxTime = Get time of maximum... minTime endTime Hertz Parabolic
			select TextGrid 'name$'
			Insert point... 4 minTime L
			# if no point exists at maxTime, insert H, otherwise skip
			if maxTime > minTime
				Insert point... 4 maxTime H
			endif
		# predicates
		elsif (label$ = "aaraam" or label$ = "kheDvaanu" or label$ = "nedvaanu" or label$ = "aaraam" or label$ = "kasrat")
			startTime = Get starting point... 1 'k'
			endTime = Get end point... 1 'k'	
			#writeInfoLine: label$
			select Pitch 'name$'
			#writeInfoLine: name$
			minTime = Get time of minimum... startTime endTime Hertz Parabolic
			# to prevent subject H = predicate L 
			minTimeN = minTime + 0.001
			maxTime = Get time of maximum... minTimeN endTime Hertz Parabolic
			select TextGrid 'name$'
			Insert point... 4 minTimeN L
			# if no point exists yet at maxTime, insert H, otherwise skip
			if maxTime > minTimeN
				Insert point... 4 maxTime H
			endif
		endif
	endfor	
	select TextGrid 'name$'
	Write to text file... 'dirName$''name$'.TextGrid
	plus Sound 'name$'
	plus Pitch 'name$'
	Remove
endfor

# clean up
select Strings fileList
Remove

