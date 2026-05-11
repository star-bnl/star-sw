#!/usr/bin/python
import sys

f=open(sys.argv[1])
l=f.readlines()

totalenergies = []
for line in l:
	# start parsing new event
	if "<event>" in line:
		totalenergies.append([])
		
	# only use lines where particles are defines
	args = line.split()
	if len(args) < 13: continue

	# select onbly final particles (-9 is the beam, 2 is a particle which decayed and 1 is final)
	if int(args[1])!=1: continue

	# add particle's energy to the list of energies in event
	totalenergies[-1].append(float(args[9]))


# print the result
print
print
count = 0
for item in totalenergies:
	print "Event %2d"%count, "Total energy (GeV)", sum(item) 
	count+=1
	
