//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.cxx,v 1.10 2003/01/02 22:19:45 balewski Exp $
// $Log: StppLPprojectMaker.cxx,v $
// Revision 1.10  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.9  2001/11/28 23:03:42  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.8  2001/05/08 03:24:51  balewski
// *** empty log message ***
//
// Revision 1.7  2001/05/04 20:29:36  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/27 20:50:45  balewski
// *** empty log message ***
//
// Revision 1.5  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.4  2001/04/19 15:33:19  balewski
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

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"

#include "StppLPprojectMaker.h"

ClassImp(StppLPprojectMaker)


//_____________________________________________________________________________
StppLPprojectMaker::StppLPprojectMaker(const char *name):StMaker(name){
 cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
 }

//_____________________________________________________________________________
StppLPprojectMaker::~StppLPprojectMaker(){
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Init(){
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Make(){
  printf("%s-Maker is empty, remove it from the chain, JB\n",GetName());
  return kStOk;
}

//_____________________________________________________________________________
Int_t StppLPprojectMaker::Finish()
{

  //printf("%s-Finish ffffffffffffffffff\n",GetName());

  return  kStOK;
}








