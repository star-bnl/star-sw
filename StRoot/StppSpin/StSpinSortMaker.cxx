//*-- Author : Jan Balewski 
// $Id: StSpinSortMaker.cxx,v 1.6 2003/01/02 22:19:44 balewski Exp $
// $Log: StSpinSortMaker.cxx,v $
// Revision 1.6  2003/01/02 22:19:44  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.5  2001/11/28 23:03:41  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.4  2001/04/24 21:58:26  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.2  2001/04/19 15:33:17  balewski
// *** empty log message ***
//
// Revision 1.1  2001/04/13 18:04:34  balewski
// *** empty log message ***
//
// Revision 1.1  2001/04/12 15:55:21  balewski
// *** empty log message ***
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
// 
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>

#include "StChain.h"
#include "St_DataSetIter.h"

#include "StSpinSortMaker.h"


ClassImp(StSpinSortMaker)

//_____________________________________________________________________________
StSpinSortMaker::StSpinSortMaker(const char *name):StMaker(name){
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
}

//_____________________________________________________________________________
StSpinSortMaker::~StSpinSortMaker(){
}

//_____________________________________________________________________________
Int_t StSpinSortMaker::Init(){
  return StMaker::Init();
}


//_____________________________________________________________________________
Int_t StSpinSortMaker::Make(){
  printf("%s-Maker is empty, remove it from the chain, JB\n",GetName());
  return kStOk;
}



