#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StvTpcActive.h"
#include "StDetectorDbMaker/St_tpcRDOMasksC.h"
ClassImp(StvTpcActive)

//______________________________________________________________________________
StvTpcActive::StvTpcActive():StActorFunctor("TPCActive")
{
}
//______________________________________________________________________________
int StvTpcActive::VoluId()
{
enum {nbpads=73};
static const int tpads[nbpads]={
                             1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
			     4, 4, 5, 5, 5, 6, 6, 6, 7, 7,
			     7, 8, 8, 8, 9, 9, 9,10,10,10,
			    11,11,11,12,12,12,13,13,13,14,
			    14,15,16,17,18,19,20,21,22,23,
			    24,25,26,27,28,29,30,31,32,33,
			    34,35,36,37,38,39,40,41,42,43,
			    44,45,45};

static const int isdets[nbpads]={
                             1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 2, 1, 0, 2, 1, 0, 2, 1, 0,
			     2, 1, 0, 2, 1, 0, 2, 1, 0, 2,
			     1, 0, 2, 1, 0, 2, 1, 0, 2, 1,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			     0, 0, 2};


  int numbv[3];
  GetIPath(3,numbv);
  int tpgv  = numbv[2];
  int tpss  = numbv[1];
  mSector= tpss+12*(tpgv-1); 
  mTPad  = numbv[0];

//		tpad >nbpads (73) prompt hits
  if (mTPad  > nbpads) mTPad -= nbpads;
  mIsDet = isdets[mTPad-1];
  mTPad  = tpads [mTPad-1];
  return 100000*mIsDet+100*mSector+mTPad;
}  
//______________________________________________________________________________
int StvTpcActive::operator()( const double xyz[3])
{
static St_tpcRDOMasksC *pRdoMasks = St_tpcRDOMasksC::instance();
  VoluId();
  if (mIsDet) return 0;
  int iRdo  = pRdoMasks->rdoForPadrow(mTPad);
  int iact  = pRdoMasks->isOn(mSector, iRdo);
  if (xyz) {};
  return iact;
}
