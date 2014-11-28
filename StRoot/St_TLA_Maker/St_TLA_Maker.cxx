//*-- Author : Victor Perevoztchikov
// 
// $Id: St_TLA_Maker.cxx,v 1.18 2007/10/27 17:42:59 fine Exp $
// $Log: St_TLA_Maker.cxx,v $
// Revision 1.18  2007/10/27 17:42:59  fine
// replace the obsolete class name
//
// Revision 1.17  2007/04/28 17:57:13  perev
// Redundant StChain.h removed
//
// Revision 1.16  2006/12/19 21:59:16  fine
// replace the class StMessMgr forward declaration with the real declaration and adjust St_TLA_Maker to show how to use logger
//
// Revision 1.15  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.14  2000/06/23 16:50:07  fisyak
// remove params
//
// Revision 1.13  1999/12/19 16:07:01  perev
// Add README
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//


#include "St_TLA_Maker.h"
#include "TDataSetIter.h"
#include "StDAQMaker/StDAQReader.h"


ClassImp(St_TLA_Maker)

//_____________________________________________________________________________
/// TLA constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
St_TLA_Maker::St_TLA_Maker(const char *name):StMaker(name){
  //
}


//_____________________________________________________________________________
/// This is TLA destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
St_TLA_Maker::~St_TLA_Maker(){
  //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t St_TLA_Maker::Init(){
  // Create tables
  // Create Histograms    
   return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t St_TLA_Maker::Make(){
  //
  //  PrintInfo();
  //
  int nArray; unsigned char *Array;

 TObjectSet *os = (TObjectSet*)GetDataSet("StDAQReader");
 assert(os);
 StDAQReader *dr = (StDAQReader*)os->GetObject();
 StTPCReader *tr = dr->getTPCReader();
 int sec=0;
 for (sec=1;sec<=12;sec++){
  int padrow;
   for (padrow=1;padrow<20; padrow++) {
     int maxpad = tr->getMaxPad(padrow);
     int pad;
     for (pad=1;pad<=maxpad; pad++) {     
       int ans =  tr->getPedestals(sec,padrow,pad,nArray,Array);
       if (ans<=0) continue;
       if (!Array) continue;
       { LOG_INFO << Form(" %d %d %d %d\n",sec,padrow,pad,nArray) << endm; }

 } } }


 return kStOK;
}
