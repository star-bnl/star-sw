//*-- Author : Jan Balewski
//  
// $Id: StppLPevalMaker.cxx,v 1.3 2003/01/02 22:19:45 balewski Exp $
// $Log: StppLPevalMaker.cxx,v $
// Revision 1.3  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.2  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.1  2001/04/12 15:19:08  balewski
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
//   Obsolete
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <stdio.h>

#include "StppLPevalMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"


ClassImp(StppLPevalMaker)

//_____________________________________________________________________________
StppLPevalMaker::StppLPevalMaker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
StppLPevalMaker::~StppLPevalMaker()
{
}

//_____________________________________________________________________________
Int_t StppLPevalMaker::Init()
{

  return StMaker::Init();
}

//_____________________________________________________________________________
void StppLPevalMaker::Clear(const char *opt) 
{
  return;
}

//_____________________________________________________________________________
Int_t StppLPevalMaker::Finish()
{
  return  kStOK;
}


//_____________________________________________________________________________
Int_t StppLPevalMaker::Make()
{
  printf("%s-Maker is empty, remove it from the chain, JB\n",GetName());
  return kStOk;
}
