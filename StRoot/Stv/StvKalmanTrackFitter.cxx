#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "TMath.h"

#include "StvKalmanTrackFitter.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvFitter.h"
#include "Stv/StvStl.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
ClassImp(StvKalmanTrackFitter)
//_____________________________________________________________________________
StvKalmanTrackFitter::StvKalmanTrackFitter(const char *name):StvTrackFitter(name)
{
  memset(mBeg,0,mEnd-mBeg+1);
}  
//_____________________________________________________________________________
void StvKalmanTrackFitter::Clear(const char*)
{
 StvTrackFitter::Clear("");
}

//_____________________________________________________________________________
int  StvKalmanTrackFitter::FitTrack(StvTrack *trak,int dir)
{
///	refit or smouthe track, using the previous Kalman.
///     idir=0 moving from out to in
///     idir=1 moving from in to out 
const double kBigErrFact = 10;


  StvNodeIter it,itBeg,itEnd;
  if (dir) { //fit in ==> out
    itBeg = trak->begin();        itEnd = trak->end();
  } else   {//fit out ==> in
    itBeg = trak->end(); --itBeg; itEnd = trak->begin();--itEnd;
  }


  StvNode *node,*preNode=0,*begNode=0;
  for (it=itBeg; it!=itEnd; (dir)? ++it:--it) {
    node = *it;
    do { //pseudo loop
      if (!begNode && node->GetXi2()<1e5) begNode = node; 
      if (!begNode) break;
      if (!preNode) 	{// 1st node
        node->mPP[dir] = node->mFP[2];
        node->mPE[dir] = node->mFE[2];
        node->mPE[dir]*= kBigErrFact;
      } else 		{ // normal node
        
      
      
      
      }
    } while(0);
    preNode = node;
  }

}
