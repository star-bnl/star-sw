// $Id: StChain.cxx,v 1.16 1998/10/06 18:00:26 perev Exp $
// $Log: StChain.cxx,v $
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
//   m_DataSet points to a DataSet owned by the Maker                   //
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
//   gStChain->SetSmearMuonOpt(0);
//   gStChain->ClusterMaker()->SetGranBarrelEta(0.12);
//   gStChain->PhotonMaker()->SetMinPT(6.);
//   gStChain->TriggerMaker()->SetMuoEtaCoverage(2.8);
//
//}
//======================end of User class=================================
//
//////////////////////////////////////////////////////////////////////////

#include <TROOT.h>
#include <TChain.h>
#include <TTree.h>
#include <TBrowser.h>
#include <TClonesArray.h>
#include <TBenchmark.h>
#include "St_XDFFile.h"
#include "St_DataSetIter.h"
#include "St_FileSet.h"
#include "StChain.h"
#include "StMaker.h"
// #include "stHistBrowser.h"
// #include "StVirtualDisplay.h"

StChain *gStChain;

ClassImp(StChain)


//_____________________________________________________________________________
StChain::StChain() 
{
   SetName("StChain");
   SetTitle("The STAR default chain");
   m_Tree          = 0;
   m_Makers        = 0;
   m_Mode          = 0;
   m_DataSet       = 0;
   m_DebugLevel    = kNormal;
}

//_____________________________________________________________________________
StChain::StChain(const char *name, const char *title)
{
   SetName(name);
   SetTitle(title);
   gStChain      = this;
   m_Version     = 100;       //StChain  version number and release date
   m_VersionDate = 180698;
   m_Tree        = 0;
   m_Mode        = 0;
//   m_Display     = 0;
   m_DataSet       = 0;
   
   SetDefaultParameters();

   gROOT->GetListOfBrowsables()->Add(this,GetName());

// create the support list for the various lists of StChain objects
   m_Makers  = new TList();

}

//_____________________________________________________________________________
StChain::~StChain()
{
//   m_Makers->Delete();
//   delete m_Makers;
}


//______________________________________________________________________________
St_DataSet *StChain::GetRun(){
//  Return pointer to run. If run does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("run");
  if (run == 0) {local.Mkdir("run"); run = local("run");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetParams(){
//  Return pointer to params. If run/params does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("run/params");
  if (run == 0) {local.Mkdir("run/params"); run = local("run/params");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetGeometry(){
//  Return pointer to params. If run/params does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("run/params");
  if (run == 0) {local.Mkdir("run/params"); run = local("run/params");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetCalib(){
//  Return pointer to calib. If run/calib does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("run/calib");
  if (run == 0) {local.Mkdir("run/calib"); run = local("run/calib");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetRawData(){
//  Return pointer to raw data. If event/raw_data does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("event/raw_data");
  if (run == 0) {local.Mkdir("event/raw_data"); run = local("event/raw_data");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetData(){
//  Return pointer to raw data. If event/data does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("event/data");
  if (run == 0) {local.Mkdir("event/data"); run = local("event/data");}
  return run;
}
//______________________________________________________________________________
St_DataSet *StChain::GetGeant(){
//  Return pointer to GEANT data. If event/geant does exist then it will be creates
  St_DataSetIter local(m_DataSet);
  local.Cd(GetName());
  St_DataSet *run = local("event/geant");
  if (run == 0) {local.Mkdir("event/geant"); run = local("event/geant");}
  return run;
}
//______________________________________________________________________________
void StChain::Browse(TBrowser *b)
{
// Browse includes the various maker-made objects into the standard ROOT TBrowser
// The following picture can be done with just a staement like
//
//  TBrowser b("Event",event);  
//
// where the "event" variable should be defined somewhere in the user's code as 
//
//          St_DataSet *event;
//
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/ChainBrowser.gif"> </P> End_Html 
//

  if( b == 0) return;

  if (m_Tree) b->Add(m_Tree,m_Tree->GetName());

//  b->Add(&m_HistBrowser, "Histograms");
//  b->Add(&m_BigBang, "BigBang");

  TIter next(m_Makers);
  StMaker *maker;
  while ((maker = (StMaker*)next())) {
     b->Add(maker,maker->GetName());
  }
  b->Add(m_DataSet,m_DataSet->GetName()); 
}

//_____________________________________________________________________________
void StChain::Clear(Option_t *option)
{
//    Reset lists of event objects
   TIter next(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->Clear(option);
   }
//   if (m_Display) m_Display->Clear();
   return;
}
//_____________________________________________________________________________
St_DataSet *StChain::DataSet(Char_t *makername)
{
// find the maker by name and return its dataset
 St_DataSet *set = 0;
 if (makername) {
  set = 0;
  if (m_Makers) {
     TIter next(m_Makers);
     StMaker *xdfin;
     while (xdfin = (StMaker*) next()){if (!strcmp(xdfin->GetName(),makername)) break;}
     if (xdfin) set = xdfin->DataSet(); 
   }
 }
 return set;
}
 
//_____________________________________________________________________________
void StChain::Draw(Option_t *option)
{
//    Insert current event in graphics pad list

#if 0
    // Check if the Event Display object has been created
   if (!m_Display) {
      Error("Draw","You must create an StDisplay object first");
      return;
   }

   m_Display->Draw(option);
#endif
}

//_____________________________________________________________________________
Int_t StChain::GetEvent(Int_t event)
{
//    Read event from Tree
   if (m_Tree) m_Tree->GetEvent(event);
   m_Event = event;
   return kStOK;
} 

//_____________________________________________________________________________
Int_t StChain::Init()
{// Initialize Chain
   if (! m_DataSet) m_DataSet = new St_DataSet(GetName()); 
   if (! m_RunSet) {//
     m_RunSet = new St_DataSet("run"); 
     St_DataSetIter local(m_DataSet);
     local.Add(m_RunSet);
   }
//    Initialise makers
   TIter next(m_Makers);
   StMaker *maker;
   TObject *objfirst, *objlast;
   SafeDelete(m_EventSet);
   while ((maker = (StMaker*)next())) {
     // save last created histogram in current Root directory
      objlast = gDirectory->GetList()->Last();

     // Initialise maker
      gBenchmark->Start((const char *) maker->GetName());
      if ( maker->Init()) return kStErr;
      gBenchmark->Stop((const char *) maker->GetName());
     // Add the Maker histograms in the Maker histograms list
      if (objlast) objfirst = gDirectory->GetList()->After(objlast);
      else         objfirst = gDirectory->GetList()->First();
      while (objfirst) {
         maker->Histograms()->Add(objfirst);
         objfirst = gDirectory->GetList()->After(objfirst);
      }
   }
   // Save Run to XDF Output file if any
   if (m_FileOut) {
     St_DataSetIter next(m_DataSet);
     next.Cd(GetName());
     St_DataSet *set = next("run");
     if (set) set = next("Run");
     if (set) m_FileOut->NextEventPut(set);
   }
   return kStOK;
}

//_____________________________________________________________________________
void StChain::Paint(Option_t *option)
{
//    Paint StChain objects

//   m_Display->Paint(option);
}

//_____________________________________________________________________________
void StChain::PrintInfo()
{
//     Gives information about versions etc.
   printf("\n\n");
   printf("**************************************************************\n");
   printf("*             StChain version:%3d released at %6d         *\n",m_Version, m_VersionDate);
   printf("**************************************************************\n");
   printf("* $Id: StChain.cxx,v 1.16 1998/10/06 18:00:26 perev Exp $    \n");
   //   printf("* %s    *\n",m_VersionCVS);
   printf("**************************************************************\n");
   printf("\n\n");

//     Print info for all defined Makers
   TIter next(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->PrintInfo();
   }
}

//_____________________________________________________________________________
void StChain::FillTree()
{
//  Fill the ROOT tree, looping on all active branches


  // Now ready to fill the Root Tree
   if(m_Tree) m_Tree->Fill();
}
//_____________________________________________________________________________
void StChain::FillXDF(St_XDFFile &file)
{
//  Put current event into XDF file
   if (&file && m_DataSet) file.NextEventPut(m_DataSet);
}

//_____________________________________________________________________________
void StChain::InitChain(TChain *chain)
{
//  Initialize branch addresses for all makers in a TChain

   if (chain == 0) return;

   m_Tree = chain;

   TIter next(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->SetChainAddress(chain);
   }
}

//_____________________________________________________________________________
void StChain::MakeTree(const char* name, const char*title)
{
//  Create a ROOT tree
//  Loop on all makers to create the Root branch (if any)

   if (m_Tree) return;

   m_Tree = new TTree(name,title);

   TIter next(m_Makers);
   StMaker *maker;
   //   Save();
   //   MakeBranch();
   while ((maker = (StMaker*)next())) {
      maker->Save();
      maker->MakeBranch();
   }
}
//_____________________________________________________________________________
void StChain::MakeBranch()
{
//   Adds the list of physics objects to the ATLFast tree as a new branch
   if (m_Save == 0 || m_Tree == 0) return;

//  Make a branch tree if a branch name has been set
   Int_t buffersize = 4000;
   m_BranchName = GetName();
   m_Tree->Branch(m_BranchName.Data(),ClassName(), &gStChain, buffersize);
}


//_____________________________________________________________________________
void StChain::SetDefaultParameters()
{

//    Setters for flags and switches
}

//_____________________________________________________________________________
Int_t StChain::Make(Int_t i)
{
   m_Event = i;
// Create event 
   St_DataSetIter nextDataSet(m_DataSet);
   nextDataSet.Cd(gStChain->GetName());
   m_EventSet = nextDataSet("event");
   SafeDelete(m_EventSet);
   m_EventSet = nextDataSet.Mkdir("event");
//   Loop on all makers
   Int_t ret;
   TIter nextMaker(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)nextMaker())) {
     // Create the new DataSet for each new type of the maker if any
     const Char_t *makertype = maker->GetTitle();
     // Test the special case
     St_DataSet *makerset = 0;
     if (makertype && strlen(makertype))
         makerset = nextDataSet.Mkdir(maker->GetTitle());
     else
         makerset = m_EventSet;
  // Call Maker
     maker->SetDataSet(makerset);
     StartMaker(maker);
     ret = maker->Make();
     EndMaker(maker,ret);
     
     if (gStChain->Debug()) printf("%s %i\n",maker->GetName(),ret);
     if (ret) return ret;
     if (gStChain->Debug()) m_DataSet->ls(2);
   }
   return kStOK;
}

//_____________________________________________________________________________
void StChain::FillClone()
{
   // Fill Makers fruits clones
   
   TIter next(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      maker->FillClone();
   }
}

//_____________________________________________________________________________
Int_t StChain::Finish()
{
//    Terminate a run
//   place to make operations on histograms, normalization,etc.
   int nerr = 0;

   TIter next(m_Makers);
   StMaker *maker;
   while ((maker = (StMaker*)next())) {
      if ( maker->Finish() ) nerr++;
      gBenchmark->Print((char *) maker->GetName());
   }
   return nerr;
}
void StChain::StartMaker(StMaker *mk)
{
  gBenchmark->Start(mk->GetName());
}
void StChain::EndMaker(StMaker *mk,Int_t iret)
{
  gBenchmark->Stop (mk->GetName());

}

//_____________________________________________________________________________
void StChain::SortDown(Int_t n1, Float_t *a, Int_t *index, Bool_t down)
{
//  sort the n1 elements of array a.
//  In output the array index contains the indices of the sorted array.
//  if down is false sort in increasing order (default is decreasing order)
//   This is a translation of the CERNLIB routine sortzv (M101)
//   based on the quicksort algorithm

   Int_t i,i1,n,i2,i3,i33,i222,iswap,n2;
   Int_t i22 = 0;
   Float_t ai;
   n = n1;
   if (n <= 0) return;
   if (n == 1) {index[0] = 0; return;}
   for (i=0;i<n;i++) index[i] = i+1;
   for (i1=2;i1<=n;i1++) {
      i3 = i1;
      i33 = index[i3-1];
      ai  = a[i33-1];
      while(1) {
         i2 = i3/2;
         if (i2 <= 0) break;
         i22 = index[i2-1];
         if (ai <= a[i22-1]) break;
         index[i3-1] = i22;
         i3 = i2;
      }
      index[i3-1] = i33;
   }

   while(1) {
      i3 = index[n-1];
      index[n-1] = index[0];
      ai = a[i3-1];
      n--;
      if(n-1 < 0) {index[0] = i3; break;}
      i1 = 1;
      while(2) {
         i2 = i1+i1;
         if (i2 <= n) i22 = index[i2-1];
         if (i2-n > 0) {index[i1-1] = i3; break;}
         if (i2-n < 0) {
            i222 = index[i2];
            if (a[i22-1] - a[i222-1] < 0) {
                i2++;
                i22 = i222;
            }
         }
         if (ai - a[i22-1] > 0) {index[i1-1] = i3; break;}
         index[i1-1] = i22;
         i1 = i2;
      }
   }
   if (!down) return;
   n2 = n1/2;
   for (i=0;i<n1;i++) index[i]--;
   for (i=0;i<n2;i++) {
      iswap         = index[i];
      index[i]      = index[n1-i-1];
      index[n1-i-1] = iswap;
   }
}
//______________________________________________________________________________
#ifdef WIN32
void StChain::Streamer(TBuffer &R__b)
{
   // Stream an object of class StChain.

   if (R__b.IsReading()) {
      R__b.ReadVersion(); //  Version_t R__v = R__b.ReadVersion();
      StMaker::Streamer(R__b);
      if (!gStChain) gStChain = this;
      gROOT->GetListOfBrowsables()->Add(this,"StChain");
      R__b >> m_Version;
      R__b >> m_VersionDate;
      R__b >> m_Run;
      R__b >> m_Event;
      R__b >> m_Mode;
      m_Tree = (TTree*)gDirectory->Get("T");
      R__b >> m_Makers;
 //     m_HistBrowser.Streamer(R__b);
   } else {
      R__b.WriteVersion(StChain::IsA());
      StMaker::Streamer(R__b);
      R__b << m_Version;
      R__b << m_VersionDate;
      R__b << m_Run;
      R__b << m_Event;
      R__b << m_Mode;
      m_Tree->Write();
      R__b << m_Makers;
//      m_HistBrowser.Streamer(R__b);
   }
}
#endif
