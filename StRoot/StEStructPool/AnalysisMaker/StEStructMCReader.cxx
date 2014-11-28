/**********************************************************************
 *
 * $Id: StEStructMCReader.cxx,v 1.10 2012/11/16 21:19:07 prindle Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:
 *   Read in STAR standard MC rootuples, and apply track and event cuts
 *   The output is StEStructEvent.
 *
 **********************************************************************
 *
 * $Log: StEStructMCReader.cxx,v $
 * Revision 1.10  2012/11/16 21:19:07  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.9  2009/01/26 14:40:43  fisyak
 * Add missing (in ROOT 5.22) includes
 *
 * Revision 1.8  2006/04/06 00:53:59  prindle
 *   Tried to rationalize the way centrality is defined.
 *   Now the reader gives a float to StEStructEvent and this float is
 * what is being used to define centrality. When we need a centrality
 * bin index we pass this number into the centrality singleton object.
 *
 * Revision 1.7  2006/02/22 22:03:19  prindle
 * Removed all references to multRef
 *
 * Revision 1.6  2004/03/19 21:42:48  chunhuih
 * calculate pseudorapidity instead of rapidity for eta used in track cuts
 *
 * Revision 1.5  2004/03/18 18:35:48  chunhuih
 *
 * use const char * instead of char * for the constructor argument filelistfile.
 *
 * Revision 1.4  2004/03/05 23:48:11  chunhuih
 *
 * test the number of entries for the given chain: avoid integer overflow.
 * use GetEntries() instead of GetEntriesFast() for TChain.
 *
 * Revision 1.3  2004/03/03 23:16:00  chunhuih
 *
 * removed the code in the constructor that gets the total number of events
 * to reduce initialization time.
 *
 * Revision 1.2  2004/03/02 21:34:47  chunhuih
 *
 * added impact parameter information to the StEStructEvent
 *
 * Revision 1.1  2004/02/26 20:06:25  chunhuih
 * initial import
 *
 *
 **********************************************************************/
#include "StEStructMCReader.h"
#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StMessMgr.h"
#include "TMath.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include <fstream>

ClassImp(StEStructMCReader)

StEStructMCReader::StEStructMCReader(TTree *tree) : meventsToDo(0), meventCount(0), mloopIndex(0), mAmDone(false), mNentries(0), mIPMAX(1000000) {
  Init(tree);
}

StEStructMCReader::StEStructMCReader(int nevents, TTree *tree, StEStructEventCuts *ecuts, StEStructTrackCuts *tcuts) : meventsToDo(nevents), meventCount(0), mloopIndex(0), mAmDone(false), mNentries(0), mIPMAX(1000000) {
  Init(tree);
}

StEStructMCReader::StEStructMCReader(int nevents, const char *fileListFile, StEStructEventCuts *ecuts, StEStructTrackCuts *tcuts) : meventsToDo(nevents), meventCount(0), mloopIndex(0), mAmDone(false), mNentries(0), mIPMAX(1000000) {
  ifstream fin(fileListFile);
  char s[256];
  TChain *chain = new TChain("h999");
  while(fin >> s ) {
    chain->Add(s);
  }
  Init((TTree *)chain);
}

Int_t StEStructMCReader::LoadTree(Int_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Int_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->IsA() != TChain::Class()) return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

Int_t StEStructMCReader::GetEntry(Int_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}

void StEStructMCReader::Init(TTree *tree)
{
//   Set branch addresses
   if (tree == 0) return;

   fChain    = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("itrac",&itrac);
   fChain->SetBranchAddress("istat",&istat);
   fChain->SetBranchAddress("ipdg",&ipdg);
   fChain->SetBranchAddress("moth1",&moth1);
   fChain->SetBranchAddress("moth2",&moth2);
   fChain->SetBranchAddress("idau1",&idau1);
   fChain->SetBranchAddress("idau2",&idau2);
   fChain->SetBranchAddress("Pxyz",Pxyz);
   fChain->SetBranchAddress("ener",&ener);
   fChain->SetBranchAddress("mass",&mass);
   fChain->SetBranchAddress("Vxyz",Vxyz);
   fChain->SetBranchAddress("Vtime",&Vtime);
   Notify();

   // get number of entries in this chain
   if(fChain != 0) {
    double x = fChain->GetEntries();
    if( x > (pow(2., 8. * sizeof(int) - 1.) - 1.) )
      gMessMgr->Message("Number of Entries causes integer overflow.", "E");
    else
      mNentries = int(x);
  }
}

Bool_t StEStructMCReader::Notify()
{
   // Called when loading a new file.
   // Get branch pointers.
   b_itrac = fChain->GetBranch("itrac");
   b_istat = fChain->GetBranch("istat");
   b_ipdg = fChain->GetBranch("ipdg");
   b_moth1 = fChain->GetBranch("moth1");
   b_moth2 = fChain->GetBranch("moth2");
   b_idau1 = fChain->GetBranch("idau1");
   b_idau2 = fChain->GetBranch("idau2");
   b_Pxyz = fChain->GetBranch("Pxyz");
   b_ener = fChain->GetBranch("ener");
   b_mass = fChain->GetBranch("mass");
   b_Vxyz = fChain->GetBranch("Vxyz");
   b_Vtime = fChain->GetBranch("Vtime");
   return kTRUE;
}

void StEStructMCReader::Show(Int_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t StEStructMCReader::Cut(Int_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}

void StEStructMCReader::Loop()
{
  if (fChain == 0) return;

  Int_t nentries = Int_t(fChain->GetEntries());

  Int_t nbytes = 0, nb = 0;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
  }
}

StEStructMCReader::~StEStructMCReader()
{
  if (!fChain) return;
  delete fChain->GetCurrentFile();
}

int StEStructMCReader::getTotalEventCount() {
  if (fChain == 0) return 0;

  double nentries = fChain->GetEntries();

  Int_t nbytes = 0, nb = 0;
  int retVal = 0;
  for (Int_t jentry=0; jentry<nentries;jentry++) {
    Int_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    if(itrac == -1) retVal++;
  }
  return retVal;
}

int StEStructMCReader::getCharge(int pid) {
  if(pid == -11 || pid == -13) return 1;
  else if(pid == 11 || pid == 13) return -1;
  else if(pid > 0) return  1;
  else if(pid < 0) return -1;
  return 0;
}

bool StEStructMCReader::measureable(int pid){
  bool retVal=false;
  if(pid<0)pid*=-1;

  switch(pid){

  case 211:
    {   // charged pion
       retVal=true;
       break;
    }
  case 321:
    {   // charged kaon
      retVal=true;
      break;
    }
  case 2212:
    {   // proton
      retVal=true;
      break;
    }
  case 11:
    {   // electron
      retVal=true;
      break;
    }
  default:
    {
      break;
    }
  }
  return retVal;
}

float* StEStructMCReader::globalDCA(float* p, float* v){

  // assumes primaryVertex at origin 
  float* r=new float[4];
  r[0]=r[1]=r[2]=r[3]=0;

  // a is the component of v vector that is parallel to p.
  float a = v[0] * p[0] + v[1] * p[1] + v[2] * p[2] ;
  a = a / sqrt(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);
  // r is the component of v vector perpendicular to p.
  r[3] = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2] - a*a);
  // Since only the magnitude of globalDCA is used,
  // leave r[0], r[1], r[2] to be 0 here.
  return r;
}

StEStructEvent* StEStructMCReader::next() {
  if( fChain == NULL || (meventsToDo != 0 && meventCount == meventsToDo) ||
      mloopIndex >= mNentries ) {
    mAmDone = true;
    return (StEStructEvent *)NULL;
  }
  StEStructEvent *retVal = new StEStructEvent();

  fillTracks(retVal);

  bool useEvent = mECuts->goodCentrality(retVal->Centrality());
  if(!useEvent) {
    delete retVal;
    retVal = NULL;
  }
  else {
    retVal->FillChargeCollections();
  }
  mECuts->fillHistogram(mECuts->centralityName(), retVal->Centrality(), useEvent);
  return retVal;
}

//--------------------------------------------------------------------------
void StEStructMCReader::fillTracks(StEStructEvent* estructEvent) {

  mnumTracks=0;
  StEStructTrack* eTrack= new StEStructTrack();

  Int_t nbytes = 0, nb = 0;
  itrac = 0;
  for(; mloopIndex < mNentries && itrac != -1; mloopIndex++) {
    Int_t ientry = LoadTree(mloopIndex);
    if(ientry < 0) break;
    nb = fChain->GetEntry(mloopIndex); nbytes += nb;
    eTrack->SetInComplete();
    if(itrac == -1 ) {
      meventCount++;
    }
    // if(istat == 11 && ipdg == mIPMAX - 1):
    // this event header contains:
    // Pxyz[0]: run ID
    // Pxyz[1]: evt ID
    // Pxyz[2]: Date
    if(istat == 11 && ipdg == mIPMAX - 2) {
      // this header contains:
      // Pxyz[0]: Impact Parameter
      // Pxyz[1]: 
      // Pxyz[2]: 
      estructEvent->SetCentrality(Pxyz[0]);
    }
    if(istat == 11 && ipdg == mIPMAX - 3) {
      // this header contains:
      // Pxyz[0]: A1
      // Pxyz[1]: Z1
      // Pxyz[2]: A2
    }
    if(istat != 1 || !measureable(ipdg) ) continue;

    float pt = TMath::Sqrt(Pxyz[0] * Pxyz[0] + Pxyz[1] * Pxyz[1]);
    if( pt < 0.15 ) continue;

    float *gdca = globalDCA(Pxyz, Vxyz);
    bool useTrack = true;
    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);

    float theta = acos(Pxyz[2] / TMath::Sqrt(Pxyz[2] * Pxyz[2] + pt * pt));
    float eta = -log(tan(theta/2.));
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    float phi=atan2((double)Pxyz[1], (double)Pxyz[0]);
    useTrack=(mTCuts->goodPhi(phi) && useTrack);
    if(useTrack)mnumTracks++;

    useTrack=(mTCuts->goodPt(pt) && useTrack);

    float _r=pt/0.139;
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);

    mTCuts->fillHistograms(useTrack);

    eTrack->SetBx(gdca[0]);
    eTrack->SetBy(gdca[1]);
    eTrack->SetBz(gdca[2]);
    eTrack->SetBxGlobal(gdca[0]);
    eTrack->SetByGlobal(gdca[1]);
    eTrack->SetBzGlobal(gdca[2]);

    delete [] gdca;
    if(!useTrack) continue;

    eTrack->SetPx(Pxyz[0]);
    eTrack->SetPy(Pxyz[1]);
    eTrack->SetPz(Pxyz[2]);
    eTrack->SetEta(eta);
    eTrack->SetPhi(phi);
    eTrack->SetCharge(getCharge(ipdg));
    estructEvent->AddTrack(eTrack);
  }
  delete eTrack;
  return;
}    
