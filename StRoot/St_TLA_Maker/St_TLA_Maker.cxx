//*-- Author : Victor Perevoztchikov
// 
// $Id: St_TLA_Maker.cxx,v 1.13 1999/12/19 16:07:01 perev Exp $
// $Log: St_TLA_Maker.cxx,v $
// Revision 1.13  1999/12/19 16:07:01  perev
// Add README
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_Maker class for Makers                                        //
//                                                                      //
// This commented block at the top of the source file is considered as  //
// this class description to be present on the this class Web page.     //
//  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html                    //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_TLA_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"


ClassImp(St_TLA_Maker)

//_____________________________________________________________________________
St_TLA_Maker::St_TLA_Maker(const char *name):StMaker(name){
 //  TLA constructor
 //
 //  const char *name -  the name of this constructor
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
}
//_____________________________________________________________________________
St_TLA_Maker::~St_TLA_Maker(){
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
Int_t St_TLA_Maker::Init(){
 //  Init - is a first method the top level StChain calls to initialize all its makers
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //

// Create tables
   St_DataSetIter       local(GetDataBase("params"));
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_TLA_Maker::Make(){
 //
 //  Make - this methoid is called in loop for each event
 //
 //  The first comment lines after the opening bracket
 //  ({) of a member function are considered as a member function description 
 //  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html   //
 //  PrintInfo();
 int nArray; unsigned char *Array;

 St_ObjectSet *os = (St_ObjectSet*)GetDataSet("StDAQReader");
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
       printf(" %d %d %d %d\n",sec,padrow,pad,nArray);

 } } }


 return kStOK;
}










