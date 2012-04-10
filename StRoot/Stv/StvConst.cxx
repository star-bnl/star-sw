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

  mXi2Hit = 22;		//Xi2 to accept new hit
  mXi2Vtx = 55;		//Xi2 to accept vertex
  mXi2Joi = 33;		//Xi2 in Refit join left & right subtrack
  mXi2Hlx = mXi2Hit*9;  //Xi2 in Helix, .
  mRxyMax = 300;	//Max radius for tracking
  mZMax   = 300;	//Max Z      for tracking
  mDca2dZeroXY 	= 6;	//max 2d dca to X=Y=0  for primary track
  mDca3dVertex 	= 3;	//max 3d dca to vertex for primary track
  mMaxCurv     	= 0.1;	//Maximal allowed curvature
  mMinP2       	= 0.003*0.003;	//Geant3 cut for too small momentum**2	
  mMaxWindow   	= 3.;	//Maximal window to search hits
  mMinHits 	= 5;	//Min number of hits allowed
  mGoodHits 	=15;	//Good number of hits allowed


}
