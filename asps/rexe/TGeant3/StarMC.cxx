/**************************************************************************
 * Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 *                                                                        *
 * Author: The ALICE Off-line Project.                                    *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

/*
$Log: StarMC.cxx,v $
Revision 1.4  2005/02/07 21:08:50  fisyak
rename antique TGeant3 to TGiant3

Revision 1.3  2004/03/01 16:02:49  fisyak
new interface to simulation based on starsim

Revision 1.2  2000/04/23 19:18:09  fisyak
Merge with Alice V3.03

Revision 1.1  2000/01/04 16:04:02  fisyak
move TGiant3 to root4star executable

Revision 1.1  1999/12/07 15:44:25  fisyak
Add geane, new TGiant3 from Starce

Revision 1.2  1999/09/29 09:24:29  fca
Introduction of the Copyright and cvs Log

*/

#include "StarMC.h"
#include "TBuffer.h"
#include "TMemberInspector.h"
ClassImp(StarMC)

StarMC* StarMC::fgMC=0;

StarMC* gMC;

StarMC::StarMC()
{
}

StarMC::StarMC(const char *name, const char *title) : TNamed(name,title),  fRandom(0)
{
  if(fgMC) {
    printf("Cannot initialise twice MonteCarlo class\n");
  } else {
    fgMC=this;
    gMC=this;
  }
  fRandom = gRandom;
}

//_____________________________________________________________________________
void StarMC::SetRandom(TRandom* random) 
{ 
// 
// Set random number generator.
//

   fRandom = random; 
}

