#!/usr/bin/python
# Script to conver the Pythia 6 codes in decay.dec to Pythia 8

def convertFile(inFileName, outFileName):

    inFile = open(inFileName, 'r')
    outFile = open(outFileName, 'w')
    
    line = inFile.readline()
    
    nPythiaLines = 0

    while line:

        gotLine = False
        iWord = 0
        words = line.split()
        
        nWords = len(words)
        for i in range(nWords):
            aWord = words[i]
            if (aWord == "PYTHIA"):
                gotLine = True
                iWord = i
                break

        pInt = 0
        qInt = 0
        jWord = 0
        gotDecay = False

        if (gotLine == True):
            print 'Line {0} has PYTHIA at word number {1}'.format(line, iWord)

            # Check what the next word is after PYTHIA on this line
            jWord = iWord+1
            if (jWord < nWords):
                bWord = words[jWord]
                # Remove trailing semi-colon
                cWord = bWord.replace(";", "")

                if (cWord.isdigit()):
                    pInt = int(cWord)
                    qInt = convertCode(pInt)
                    print 'Pythia integer number is {0}, new value is {1}'.format(pInt, qInt)
                    gotDecay = True
                    nPythiaLines += 1
                else:
                    print 'Ignoring word {0}'.format(cWord)

        if (gotDecay == True):

            # We have a valid Pythia decay line
            tmpDecay = []
            for i in range(jWord):
                tmpDecay.append(words[i])
            tmpDecay.append(qInt)
            tmpDecay.append(";")
            print 'Decay line = {0}'.format(tmpDecay)

            theDecay = checkDecayMode(pInt, tmpDecay)
            print 'New Decay line = {0}'.format(theDecay)

            # Print the adjusted Pythia decay line in the output file
            newPythiaLine = ''
            nDecay = len(theDecay)
            nDecay1 = nDecay - 2
            for k in range(nDecay):
                newPythiaLine += str(theDecay[k])
                if (k < nDecay1): 
                    newPythiaLine += ' '
            newPythiaLine += '\n'
            outFile.write(newPythiaLine)

        else:
            # Just print the line we have in the output file
            outFile.write(line)

        # Read the next line
        line = inFile.readline()

    print 'nPythiaLines = {0}'.format(nPythiaLines)

    outFile.close()
    inFile.close()


def checkDecayMode(oldCodeInt, tmpDecay):

    theDecay = []

    if oldCodeInt == 33:
        # We may have a g q q' mode. Change this to q q'.
        for i in range(len(tmpDecay)):
            word = tmpDecay[i]
            if word != "g":
                # Exclude the gluon word
                print 'Appending the word {0} for oldCodeInt = 33'.format(word)
                theDecay.append(word)
    else:
        theDecay = tmpDecay

    return theDecay


def convertCode(tmpModeInt):

    modeInt = tmpModeInt

    if tmpModeInt == 0:
      modeInt = 0; # phase-space
    elif tmpModeInt == 1:
      modeInt = 1; # omega or phi -> 3pi
    elif tmpModeInt == 2:
      modeInt = 11; # Dalitz decay
    elif tmpModeInt == 3:
      modeInt = 2; # V -> PS PS
    elif tmpModeInt == 4:
      modeInt = 92; # onium -> ggg or gg gamma
    elif tmpModeInt == 11:
      modeInt = 42; # phase-space of hadrons from available quarks
    elif tmpModeInt == 12:
      modeInt = 42; # phase-space for onia resonances
    elif tmpModeInt == 13:
      modeInt = 43; # phase-space of at least 3 hadrons
    elif tmpModeInt == 14:
      modeInt = 44; # phase-space of at least 4 hadrons
    elif tmpModeInt == 15:
      modeInt = 45; # phase-space of at least 5 hadrons
    elif tmpModeInt >= 22 and tmpModeInt <= 30:
      modeInt = tmpModeInt + 40; # phase space of hadrons with fixed multiplicity (modeInt - 60)
    elif tmpModeInt == 31:
      modeInt = 42; # two or more quarks phase-space; one spectactor quark
    elif tmpModeInt == 32:
      modeInt = 91; # qqbar or gg pair
    elif tmpModeInt == 33:
      modeInt = 91; # triplet q X qbar, where X = gluon or colour singlet (remove gluon, set to 91)
    elif tmpModeInt == 41:
      modeInt = 21; # weak decay phase space, weighting nu_tau spectrum
    elif tmpModeInt == 42:
      modeInt = 22; # weak decay V-A matrix element
    elif tmpModeInt == 43:
      modeInt = 22; # weak decay V-A matrix element, quarks as jets (superfluous)
    elif tmpModeInt == 44:
      modeInt = 22; # weak decay V-A matrix element, parton showers (superfluous)
    elif tmpModeInt == 48:
      modeInt = 23; # weak decay V-A matrix element, at least 3 decay products
    elif tmpModeInt == 50:
      modeInt = 0; # default behaviour
    elif tmpModeInt == 51:
      modeInt = 0; # step threshold (channel switched off when mass daughters > mother mass)
    elif tmpModeInt == 52 or tmpModeInt == 53:
      modeInt = 0; # beta-factor threshold
    elif tmpModeInt == 84:
      modeInt = 42; # unknown physics process - just use phase-space
    elif tmpModeInt == 101:
      modeInt = 0; # continuation line
    elif tmpModeInt == 102:
      modeInt = 0; # off mass shell particles.

    return modeInt


if __name__ == "__main__":

    inFileName = 'DECAY.DEC'
    outFileName = 'newDECAY.DEC'
    convertFile(inFileName, outFileName)
