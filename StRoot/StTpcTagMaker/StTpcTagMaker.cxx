//*-- Author : Iwona Sakrejda
// 
// $Id: StTpcTagMaker.cxx,v 1.2 2000/05/24 00:25:50 sakrejda Exp $
// $Log: StTpcTagMaker.cxx,v $
// Revision 1.2  2000/05/24 00:25:50  sakrejda
// Body of the Maker and the header cleaned up, comments added.
//
// Revision 1.1  2000/05/24 00:07:02  sakrejda
// Maker to fill TPC reconstruction quality flags created
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcTagMaker class for TPC Reconstruction Tags                      //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTpcTagMaker.h"
#include "StChain.h"


ClassImp(StTpcTagMaker)

//_____________________________________________________________________________
StTpcTagMaker::StTpcTagMaker(const char *name):StMaker(name){
 //  StTpcTagMaker constructor
 //
 //  const char *name -  the name of this constructor
 //
}
//_____________________________________________________________________________
StTpcTagMaker::~StTpcTagMaker(){
  // StTpcTagMaker destructor
 //
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Init(){
 //  Init - is a first method the top level StChain calls to initialize all its makers
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Make(){
 //
 //  Make - this methoid is called in loop for each event
 //
 return kStOK;
}










