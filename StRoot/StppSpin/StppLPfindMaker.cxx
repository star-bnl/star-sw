//*-- Author : Jan Balewski
//  
// JB 3/30/01 - divorce with MC. Only StEvent is used. No evaluation
//
// $Id: StppLPfindMaker.cxx,v 1.9 2003/01/02 22:19:45 balewski Exp $
// $Log: StppLPfindMaker.cxx,v $
// Revision 1.9  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.8  2001/11/28 23:03:41  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.7  2001/06/07 17:02:52  balewski
// *** empty log message ***
//
// Revision 1.6  2001/05/03 23:38:10  balewski
// *** empty log message ***
//
// Revision 1.5  2001/04/27 20:50:45  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/26 20:04:52  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/23 19:44:26  balewski
// *** empty log message ***
//
// Revision 1.2  2001/04/23 15:02:10  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.5  2001/04/13 20:15:13  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/12 21:05:46  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
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

//_____________________________________________________________________________
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


