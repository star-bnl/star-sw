// $Id: StChain.cxx,v 1.37 1999/06/11 17:45:56 perev Exp $
// $Log: StChain.cxx,v $
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
// Each Maker is responsible for creating its branch of the Tree.       //
// The following table shows the list of makers currently implemented   //
// The default option to Save the Maker info in the Tree is mentioned.  //
//                                                                      //
//    Maker name        Save in Tree                                    //
//    ==========        ============                                    //
//   xdfin              event/geant                                     //
//   evg_Maker          event                                           //
//   tss_Maker          event/raw_data/tpc                              //
//   srs_Maker          event/raw_data/svt                              //
//   tcl_Maker          event/data/tpc/hits                             //
//   tpt_Maker          event/data/tpc/tracks                           //
//                                                                      //
//                                                                      //
// Makers must derive from the base class StMaker.                      //
// StMaker provides a common interface to all Makers.                   //
// Each Maker is responsible for defining its own parameters and        //
// histograms.                                                          //
// Each Maker has its own list of histograms.                           //
// Each Maker has an associated companion class corresponding to the    //
// type of physics object reconstructed by the Maker.                   //
// For example, St_tcl_Maker     fill   event/data/tpc/hits DataSet     //
//              St_tpt_Maker     fill   event/data/tpc/tracks DataSet   // 
// The pointer supporting the created object(s) is defined in StMaker   //
//   GetDataSet() points to a DataSet owned by the Maker                   //
//                                                                      //
// The function StChain::Maketree must be called after the creation     //
// of the StChain object to create a Root Tree (not yet implemented).   //
//                                                                      //
// An example of main program/macro to use StChain is given below:      //
//========================================================================
//void umain(Int_t nevents=100)
//{
//   gROOT->Reset();
//   gSystem->Load("libStChain.so");  // dynamically link the compiled shared library
//
//   // Open the root output file
//   TFile file("StChain.root","recreate","StChain root file",2);
//   St_XDFFile xdffile_in("StChain_in.xdf","r"); // Open XDF file to read  event
//   St_XDFFile xdffile_out("StChain.xdf","w");     // Open XDF file to write event
//   
//   StChain chain("StChain");     // create main object to run StChain
//   St_xdfin_Maker xdfin("XdfIn","test"); // create xdfin object to run in StChain
//   St_evg_Maker evg_Maker("evg_Maker","event"); // event generator
//   St_tss_Maker tss_Maker("tss_Maker","event/raw_data/tpc"); // TPC slow simulator
//   St_srs_Maker srs_Maker("srs_Maker","event/data/svt"); // SVT fast simulator
//   St_tcl_Maker tcl_Maker("tcl_Maker","event/data/tpc/hits"); // TPC clustering
//   St_tpt_Maker tpt_Maker("tpt_Maker","event/data/tpc/tracks"); // TPVC tracking
//   St_xdfout_Maker xdfout("XdfOut","test"); // create xdfin object to run in StChain
//   chain.SetInputXDFile(&xdffile_in);      // pass file to xdfin
//   chain.SetOutputXDFile(&xdffile_out);    // pass file to xdfout
//
//   User user;           // create an object of the User class defined in user.C
//
//   chain.Init();      // Initialise event (maker histograms,etc)
//   chain.MakeTree();  // Create the Root tree
//
//   gROOT->LoadMacro("user.C");  // compile/interpret user file
//
//   for (Int_t i=0; i<nevents; i++) {
//      if (i%100 == 0) printf("In loop:%d\n",i);
//      chain.Make(i);       // Generate and reconstruct event
//      user.FillHistograms(); // User has possibility to decide if store event here!
//      chain.FillTree();
//||    chain.FillXDF(xdffile_out); 
//||    xdffile.NextEventPut(chain.DataSet()); 
//      chain.Clear();       // Clear all event lists
//   }
//   chain.Finish();
//
//   // save objects in Root file
//   chain.Write();  //save main StChain object (and run parameters)
//   xdffile_out.CloseXDF();  
//}
//========================================================================
//                                                                      //
// This example illustrates how to:                                     //
//    - Load a shared library                                           //
//    - Open a Root file                                                //
//    - Initialise StChain                                              //
//    - Load some user code (interpreted)                               //
//      This user code may redefine some Maker parameters               //
//    - Make a loop on events                                           //
//    - Save histograms and the main StChain object and its Makers      //
//                                                                      //
//========================================================================
//  An example of a User class is given below:                          //
//========================================================================
//
//#ifndef user_H
//#define user_H
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// User                                                                 //
//                                                                      //
// Example of a user class to perform user specific tasks when running  //
// the StChain program.                                                 //
//                                                                      //
// This class illustrates:                                              //
//   - How to set run parameters                                        //
//   - How to create and fill histograms                                //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
//class TH1F;
//class StChain;
//class StClusterMaker;
//class StPhotonMaker;
//
//class User {
//
//private:
//   TH1F             *m_hist1;       //pointer to histogram
//   TH1F             *m_hist2;       //pointer to histogram
//   TH1F             *m_hist3;       //pointer to histogram
//public:
//               User();
//   void        FillHistograms();
//   void        SetRunParameters();
//
//#endif
//};
//
//_________________________________________________________________________
//User::User() 
//{
//   SetRunParameters();  //change default parameters
//
//         Create a few histograms
//   m_hist1 = new TH1F("hist1","Number of tracks per event",100,0,100);
//   m_hist2 = new TH1F("hist2","Number of clusters",100,0,100);
//   m_hist3 = new TH1F("hist3","Number of isolated muons",20,0,20);
//}
//
//_________________________________________________________________________
//void User::FillHistograms()
//{
////   m_hist1.Fill(event->GetNtracks());
////   m_hist2.Fill(event->GetNclusters));
////   m_hist3.Fill(event->GetNIsoMuons());
//}
//
//_________________________________________________________________________
//void User::SetRunParameters()
//{
//  // change StChain default parameters
//
//   fgStChain->SetSmearMuonOpt(0);
//   fgStChain->ClusterMaker()->SetGranBarrelEta(0.12);
//   fgStChain->PhotonMaker()->SetMinPT(6.);
//   fgStChain->TriggerMaker()->SetMuoEtaCoverage(2.8);
//
//}
//======================end of User class=================================
//
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TChain.h"
#include "TTree.h"
#include "TBrowser.h"
#include "TClonesArray.h"
#include "TBenchmark.h"
#include "TSystem.h"
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "StChain.h"
#include "StMaker.h"
// #include "stHistBrowser.h"
// #include "StVirtualDisplay.h"

StChain *fgStChain;

ClassImp(StChain)
//_____________________________________________________________________________
const char  *StChain::GetCVSIdC()
{static const char cvs[]="$Id: StChain.cxx,v 1.37 1999/06/11 17:45:56 perev Exp $";
 return cvs;};

//_____________________________________________________________________________
StChain::StChain():StMaker("StChain") 
{
   m_Tree          = 0;
   m_Mode          = 0;
   m_DebugLevel    = kNormal;
}

//_____________________________________________________________________________
StChain::StChain(const char *name):
StMaker(name)
{
   m_Version     = 100;       //StChain  version number and release date
   m_VersionDate = 180698;
   
   m_Tree        = 0;
   m_Mode        = 0;
//   m_Display     = 0;

   gROOT->GetListOfBrowsables()->Add(this,GetName());

}

//_____________________________________________________________________________
StChain::~StChain()
{
}
//_____________________________________________________________________________
void StChain::Clear(Option_t *option)
{
//    Reset lists of event objects
   TIter next(GetMakeList());
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->Clear(option);
   }
//   if (m_Display) m_Display->Clear();
   return;
}
//_____________________________________________________________________________
void StChain::PrintInfo()
{
//     Gives information about versions etc.
   printf("\n\n");
   printf("**************************************************************\n");
   printf("*             StChain version:%3d released at %6d         *\n",m_Version, m_VersionDate);
   printf("**************************************************************\n");
   printf("* $Id: StChain.cxx,v 1.37 1999/06/11 17:45:56 perev Exp $    \n");
   //   printf("* %s    *\n",m_VersionCVS);
   printf("**************************************************************\n");
   printf("\n\n");

//     Print info for all defined Makers
   TIter next(GetMakeList());
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->PrintInfo();
   }
}
void StChain::Streamer(TBuffer &)
{ Error("Streamer"," attempt to write %s\n ",GetName());
  assert(0);
}

