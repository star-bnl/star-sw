//*-- Author : Iwona Sakrejda
// 
// $Id: StTpcTagMaker.cxx,v 1.1 2000/05/24 00:07:02 sakrejda Exp $
// $Log: StTpcTagMaker.cxx,v $
// Revision 1.1  2000/05/24 00:07:02  sakrejda
// Maker to fill TPC reconstruction quality flags created
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcTagMaker class for Makers                                       //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StTpcTagMaker.h"
#include "StChain.h"


ClassImp(StTpcTagMaker)

//_____________________________________________________________________________
StTpcTagMaker::StTpcTagMaker(const char *name):StMaker(name){
 //  TLA constructor
 //
 //  const char *name -  the name of this constructor
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
StTpcTagMaker::~StTpcTagMaker(){
 // This TLA destructor
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Init(){
 //  Init - is a first method the top level StChain calls to initialize all its makers
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTpcTagMaker::Make(){
 //
 //  Make - this methoid is called in loop for each event
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
 //  PrintInfo();

 return kStOK;
}










