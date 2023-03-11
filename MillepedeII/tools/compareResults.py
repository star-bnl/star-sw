#!/usr/bin32/python

## \file
# Compare two millepede results files
#
# \author Claus Kleinwort, DESY, 2014 (Claus.Kleinwort@desy.de)
#
#  \copyright
#  Copyright (c) 2014 - 2022 Deutsches Elektronen-Synchroton,
#  Member of the Helmholtz Association, (DESY), HAMBURG, GERMANY \n\n
#  This library is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Library General Public License as
#  published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version. \n\n
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Library General Public License for more details. \n\n
#  You should have received a copy of the GNU Library General Public
#  License along with this program (see the file COPYING.LIB for more
#  details); if not, write to the Free Software Foundation, Inc.,
#  675 Mass Ave, Cambridge, MA 02139, USA.
#
# A text file is produced as ROOT input (tree->ReadFile()) containing:
# - For all labels appearing in boths files ("matched"): label value1 value2.
#   (Labels with two zero values are skipped, usually indicating fixed parameters.)
# - For all labels appearing only in one file: -label, value, value. 
#   (Labels with a zero value are skipped, usually indicating fixed parameters.)
#
# From the matched (not skipped) labels the mean and RMS is calculated for each input file
# and the correlation of both.
#
# Hardcoded defaults can be replaced by command line arguments for
#    - Input text file names
#    - Output text file name
#
import math, sys


## Combine two MP-II result files
#
# Correlate results (220518)
#
# @param[in]  name1  first result text file name
# @param[in]  name2  second result text file name
# @param[in]  name3  output text file name
#
def combineFiles(name1, name2, name3):

  # Add results from file
  def addFile(name):
    tf = open(name)
    for line in iter(tf):
      if line[0] == '*' or line[0] == '!':
        continue
      fields = line.split()
      if len(fields) < 3:
        continue
      if fields[0].isalpha():
        continue  
      label = int(fields[0])
      value = float(fields[1])
      if label not in results:
        results[label] = []
      results[label].append(value)
    tf.close()    
    
  results = {}
  # read first file
  addFile(name1)
  # read second file
  addFile(name2)

  # write output file, correlate
  sn = sx = sy = sxx = sxy = syy = 0.
  fout = open(name3, 'w')
  fout.write("label/I:value_1/F:value_2/F\n") # ROOT tree header
  for l in sorted(results.iterkeys()):
    r = results[l]
    if len(r) == 2:
      # matched label
      if r[0] <> 0. or r[1] <> 0.: 
        fout.write("%10d %12.5g %12.5g \n" % (l, r[0], r[1]))
        sn += 1.; sx += r[0]; sy += r[1]; sxx += r[0] * r[0]; sxy += r[0] * r[1]; syy += r[1] * r[1]
    elif len(r) == 1: 
      # unmatched label
      if r[0] <> 0.: 
        fout.write("%10d %12.5g %12.5g \n" % (-l, r[0], r[0]))
      print ' unmatched ', l  
  fout.close()
  
  print ' output file        ', fileNameOut
  print ' total parameters   ', len(results)
  matched = int(sn)
  if (matched <= 0): 
    return 
  
  #comparision
  print ' matched parameters ', matched
  sx /= sn; sy /= sn
  print ' mean1, mean2       ', sx, sy  
  sxx /= sn; syy /= sn
  print ' rms1, rms2         ', math.sqrt(sxx - sx * sx), math.sqrt(syy - sy * sy)
  sxy /= sn
  print ' correlation        ', (sxy - sx * sy) / (math.sqrt(sxx - sx * sx) * math.sqrt(syy - sy * sy))

          
if __name__ == '__main__':
  # input files
  fileNameIn1 = "millepede.res1"
  fileNameIn2 = "millepede.res2"
  # output file
  fileNameOut = "mp2compare.txt"
  # ## use command line arguments ?
  narg = len(sys.argv)
  if narg > 1:
    if narg < 3:
      print " usage: compareResults.py <input file name1>  <input file name2> [<output file name>]"
      sys.exit(2)
    else:
      fileNameIn1 = sys.argv[1]
      fileNameIn2 = sys.argv[2]
      if narg > 3:
        fileNameOut = sys.argv[3]

  print fileNameIn1, fileNameIn2, fileNameOut
  # combine input files 1 and 2 into output file
  combineFiles(fileNameIn1, fileNameIn2, fileNameOut)
