//*-- Author : Victor Perevoztchikov
// 
// $Id: StChainMaker.cxx,v 1.2 2000/03/23 03:45:51 fine Exp $
// $Log: StChainMaker.cxx,v $
// Revision 1.2  2000/03/23 03:45:51  fine
// Clean up
//
// Revision 1.1  1999/11/18 23:34:17  fisyak
// Add l3 chain with new clustering, add ChainMaker to remove ugly print out
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChainMaker class for Makers                                        //
//                                                                      //
// This commented block at the top of the source file is considered as  //
// this class description to be present on the this class Web page.     //
//  see: begin_html <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> end_html                    //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StChainMaker.h"
#include "StChain.h"
ClassImp(StChainMaker)

//_____________________________________________________________________________
StChainMaker::StChainMaker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
StChainMaker::~StChainMaker(){
}

