// $Id: StChain.cxx,v 1.41 1999/12/03 01:24:39 fine Exp $
// $Log: StChain.cxx,v $
// Revision 1.41  1999/12/03 01:24:39  fine
// Advanced timer has been introduced
//
// Revision 1.40  1999/07/14 15:26:18  fine
// Context menu MakeEvent method has been introduced
//
// Revision 1.39  1999/07/13 02:19:33  perev
// GetCVS,StEvtHddr,etc...
//
// Revision 1.38  1999/07/11 20:40:35  perev
// Move Clear from StChain to StMaker
//
// Revision 1.37  1999/06/11 17:45:56  perev
// assert StMaker::Streamer to forbid to write it
//
// Revision 1.36  1999/03/19 20:30:48  perev
// GetCVSTag introduced
//
// Revision 1.35  1999/03/11 01:23:58  perev
// new schema StChain
//
// Revision 1.23  1998/12/21 19:42:50  fisyak
// Move ROOT includes to non system
//
// Revision 1.22  1998/11/29 20:01:09  fisyak
// Fix typo with Run/run
//
// Revision 1.21  1998/11/25 21:58:21  fisyak
// Cleanup
//
// Revision 1.20  1998/11/22 18:28:05  fisyak
// Add name of tag
//
// Revision 1.19  1998/11/19 01:23:56  fine
// StChain::MakeDoc has been introduced, StChain::MakeDoc has been fixed (see macros/bfc_doc.C macro
//
// Revision 1.18  1998/10/31 00:21:30  fisyak
// Makers take care about branches
//
// Revision 1.17  1998/10/07 18:43:57  perev
// Add Spy classes for Farm Monitor
//
// Revision 1.16  1998/10/06 18:00:26  perev
// cleanup
//
// Revision 1.15  1998/09/23 20:22:51  fisyak
// Prerelease SL98h
//
// Revision 1.14  1998/09/16 14:29:33  love
// St_DataSetIter.h added
//
// Revision 1.13  1998/08/26 12:15:08  fisyak
// Remove asu & dsl libraries
//
// Revision 1.12  1998/08/18 14:05:01  fisyak
// Add to bfc dst
//
// Revision 1.11  1998/08/07 19:34:53  fisyak
// Add St_run_Maker
//
// Revision 1.10  1998/07/23 21:03:30  fisyak
// Add more comments
//
// Revision 1.9  1998/07/23 11:32:11  fisyak
// Add comments
//
// Revision 1.8  1998/07/20 15:08:08  fisyak
// Add tcl and tpt
//
// Revision 1.7  1998/07/19 21:16:29  fisyak
// add log information
//
// Revision 1.6  1998/07/19 21:14:48  fisyak
// add log information
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StChain                                                              //
//                                                                      //
// Main class to control the StChain program.                           //
//                                                                      //
// This class was done on the base of Begin_html <a href="http://root.cern.ch/root/Atlfast.html"> ATLFAST </a>End_Html C++ class library           //
// This class is a general framework for programs that needs to:        //
//    - Initialise some parameters                                      //
//    - Loop on events                                                  //
//    - Print results and save histograms, etc                          //
//                                                                      //
// The event processor StChain::Make loops on a list of Makers          //
// where each maker performs some task on the event data and generates  //
// results.                                                             //
// New Makers can be inserted by a user without modifying this class.   //
// Note that the order in which the Makers are called is the order      //
// of insertion in the list of Makers.                                  //
//                                                                      //
// Makers must derive from the base class StMaker.                      //
// StMaker provides a common interface to all Makers.                   //
// Each Maker is responsible for defining its own parameters and        //
// histograms.                                                          //
// Each Maker has its own list of histograms.                           //
// The pointer supporting the created object(s) is defined in StMaker   //
//   GetDataSet() points to a DataSet owned by the Maker                //
//                                                                      //
// The function StChain::Maketree must be called after the creation     //
// of the StChain object to create a Root Tree (not yet implemented).   //
//                                                                      //
//
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TBrowser.h"
#include "StChain.h"
#include "StEvtHddr.h"

StChain *fgStChain=0;

ClassImp(StChain)

//_____________________________________________________________________________
StChain::StChain(const char *name):
StMaker(name)
{
   m_Version     = 100;       //StChain  version number and release date
   m_VersionDate = 180698;
   m_EvtHddr  	= new StEvtHddr(m_ConstSet);
   gROOT->GetListOfBrowsables()->Add(this,GetName());

}

//_____________________________________________________________________________
StChain::~StChain()
{
}
void StChain::Streamer(TBuffer &)
{ Error("Streamer"," attempt to write %s\n ",GetName());
  assert(0);
}
//_____________________________________________________________________________
void StChain::Clear(Option_t *option)
{
 StartTimer();
 StMaker::Clear(option);
 StopTimer();
}
//_____________________________________________________________________________
Int_t StChain::Finish(){
 StartTimer();
 Int_t res = StMaker::Finish();
 StopTimer();
 return res;
}
//_____________________________________________________________________________
Int_t StChain::Init()
{
 StartTimer();
 Int_t res = StMaker::Init();
 StopTimer();
 return res;
}
//_____________________________________________________________________________
Int_t StChain::Make() {
 StartTimer();
 Int_t res = StMaker::Make();
 StopTimer();
 return res;
}
//_____________________________________________________________________________
Int_t StChain::MakeEvent() 
{
  // Make next event from the TBrowser TContextMenu
  Clear();
  return StMaker::Make(GetNumber()+1);
}

