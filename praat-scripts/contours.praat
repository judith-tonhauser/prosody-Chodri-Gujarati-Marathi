##############################
#
#For all .wav files and .TextGrid files in a directory:
#
# Extracts words from word tier (1)
# Extracts tones from tone tier (3) 
# Saves data in a .csv file
#
#
# Copyright (C) <2015>  <Rachel Steindel Burdin>
# modified by Judith Tonhauser in March 2018 for Bhili-Kandeshi prosody project
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
####################################

form GetToBi
	sentence wavdirectory /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-utterances/
	sentence output  /Users/tonhauser.1/Documents/current-research-topics/prosody-Khandeshi-Bhili/production-exp-data/Gujarati-contours.csv
endform

#if fileReadable (output$)
#	filedelete output$
#endif

textgriddirectory$ = wavdirectory$

list = Create Strings as file list... list 'wavdirectory$'*.wav

fileappend 'output$' File,Words,Contour 'newline$'

numberOfFiles = Get number of strings
for file to numberOfFiles
	select Strings list
	filename$ = Get string... file
	basenamelength = length(filename$) - 4
	textgrid$ = textgriddirectory$ + left$(filename$, basenamelength) + ".TextGrid"
	textgrid = Read from file... 'textgrid$'
	
	select 'textgrid'
	fileappend 'output$' 'filename$',
	#appendInfoLine: filename$

	# print all the words on tier 1
	nwords = Get number of intervals... 1
	for j from 1 to nwords
		thisWord$ = Get label of interval... 1 j
	# if the word is not empty, print it 
		if 	thisWord$ <> ""
			fileappend 'output$' 'thisWord$' 
		endif
	endfor

	fileappend 'output$' ,

	# print all the tones on tier 3
	tones = Get number of points... 3
	for j from 1 to tones
		thisTone$ = Get label of point... 3 j
				fileappend 'output$' 'thisTone$' 
	endfor
	fileappend 'output$' 'newline$'
Remove
endfor

