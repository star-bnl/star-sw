//*-- Author : Jan Balewski
//  
// $Id: StppLPfindMaker.cxx,v 1.10 2003/01/03 23:37:18 balewski Exp $
// $Log: StppLPfindMaker.cxx,v $
// Revision 1.10  2003/01/03 23:37:18  balewski
// cleanup
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <math.h>

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "TH2.h"


#include "StppLPfindMaker.h"

ClassImp(StppLPfindMaker)
//void StppLPfindMaker::Streamer(TBuffer &b){};  // do NOT change it J.B.

//_____________________________________________________________________________
StppLPfindMaker::StppLPfindMaker(const char *name):StMaker(name)
{
  cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
}
//_____________________________________________________________________________
StppLPfindMaker::~StppLPfindMaker()
{

}

//_____________________________________________________________________________
Int_t StppLPfindMaker::Init()
{
  return StMaker::Init();
}
//___________________________________________________________
Int_t StppLPfindMaker::InitRun(int runumber){

  return StMaker::Init();
}

//___________________________________________________________
Int_t StppLPfindMaker::Finish()
{
  return  kStOK;
}


//_____________________________________________________________________________
Int_t StppLPfindMaker::Make()
{
  printf("%s-Maker is a test-maker for EEMC, JB\n",GetName());
  return kStOk;
}


