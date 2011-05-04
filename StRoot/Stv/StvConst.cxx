#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "StvConst.h"

StvConst *StvConst::mgConst=0;

//______________________________________________________________________________
StvConst::StvConst(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  assert(!mgConst);
  mgConst = this;

  mXi2Hit = 55;		//Xi2 to accept new hit
  mXi2Vtx = 333;	//Xi2 to accept vertex
  mXi2Joi = mXi2Hit*10;	//Xi2 in Refit join left & right subtrack
  mXi2Big = mXi2Hit*10;	//Xi2 in Refit, temporary allowed Xi2.
  mRxyMax = 300;	//Max radius for tracking
  mZMax   = 300;	//Max Z      for tracking
  mDca2dZeroXY = 6;	//max 2d dca to X=Y=0  for primary track
  mDca3dVertex = 4;	//max 3d dca to vertex for primary track
  mMaxCurv     = 0.1;	//Maximal allowed curvature
  mMinP2       = 0.003*0.003;	//Geant3 cut for too small momentum**2	
  mMaxWindow   = 3.;	//Maximal window to search hits
  mMinHits = 10;	//Min number of hits allowed


}
