// $Id: StPeCMaker.cxx,v 1.8 2000/03/24 22:36:24 nystrand Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.8  2000/03/24 22:36:24  nystrand
// First version with StPeCEvent
//
// Revision 1.7  2000/01/20 23:03:08  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.6  1999/09/24 01:23:19  fisyak
// Reduced Include Path
//
// Revision 1.5  1999/07/15 13:57:20  perev
// cleanup
//
// Revision 1.4  1999/06/27 22:45:29  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/05/01 00:57:02  fisyak
// Change Clear function to defualt
//
// Revision 1.2  1999/04/08 16:37:15  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:27  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Maker for Peripheral Collisions DST analysis
//  This version uses StPeCEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#include "StPeCMaker.h"
#include "StEventTypes.h"
#include "StChain.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "TH1.h"

static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.8 2000/03/24 22:36:24 nystrand Exp $";


ClassImp(StPeCMaker)

StPeCMaker::StPeCMaker(const Char_t *name) : StMaker(name) {
}

StPeCMaker::~StPeCMaker() {}

Int_t StPeCMaker::Init() {

  cout<<"StPeCMaker: Initializing. Booking histograms..."<<endl;

  m_outfile   = new TFile("StPeCMaker.hist.root","recreate");
  m_hstat     = new TH1F("hstat","Statistics: Nevt, Nevt(0<Ntrk<15)",2,-0.5,1.5); 
  m_hntrk     = new TH1F("hntrk","Number of Global Tracks",50,-0.5,49.5);
  m_hnmwchts  = new TH1F("hnmwchts","#MWC Hits",50,-0.5,49.5);
  m_hnctbhts  = new TH1F("hnctbhts","#CTB Hits",50,-0.5,49.5);
  m_hnvtxtrk  = new TH1F("hnvtxtrk","#Primary Vertex Tracks",50,-0.5,49.5);
  m_hsumq     = new TH1F("hsumq","Sum Q",11,-5.5,5.5);
  m_hsumpt    = new TH1F("hsumpt","Sum Pt",50,0.0,0.75);
  m_hzvert    = new TH1F("hzvert","Z Primary Vertex (cm)",50,-60.0,60.0);
  m_hminvpi   = new TH1F("hminvpi","2-Track Evts. Minv pions",50,0.2,1.5);
  m_hminvk    = new TH1F("hminvk","2-Track Evts. Minv kaons",50,0.8,2.0);
  m_hrappi    = new TH1F("hrappi","Evt. Rapidity (pions)",50,-5.0,5.0);
  m_hrapka    = new TH1F("hrapka","Evt. Rapidity (kaons)",50,-5.0,5.0);
  cout<<"StPeCMaker: Initialization done!"<<endl;

  return StMaker::Init();
}


Int_t StPeCMaker::Make() {

  // Count all the events
  m_hstat->Fill(0.0);

  StEvent* event = new StEvent;
  event = (StEvent *) GetInputDS("StEvent");
  if (!event){
    cout<<"StPeCMaker: There was no StEvent!  Return."<<endl;
    return kStOK; 
  }

  // Do this way since call to event->summary->numberOfTracks() crashes
  StSPtrVecTrackNode& tempn = event->trackNodes();
  Int_t NTracks=tempn.size();
  if( NTracks > 15 ){
    cout<<"StPeCMaker: Number of good tracks: "<<NTracks<<endl;
    cout<<"Not a peripheral event (NTracks>15)"<<endl;
    return kStOK;
  }
  if( NTracks <= 0 ){
    cout<<"StPeCMaker: Event has no tracks!"<<endl;
    return kStOK;
  }
  // Count the events with 0<Ntrk<15
  m_hstat->Fill(1.0);

  // Instantiate StPeCEvent
  StPeCEvent *pevent = new StPeCEvent;

  // Fill StPeCEvent
  Int_t ireturn;
  ireturn = FillStPeCEvent(event,pevent);

  // Fill the PeC histograms using StPeCEvent
  ireturn = FillHistograms(pevent);

  // Example of routine how to use StPeCEvent for analysis
  ireturn = ExampleAnalysis(pevent);

  delete event; delete pevent;
  return kStOK;
}


Int_t StPeCMaker::FillStPeCEvent(StEvent *event, StPeCEvent *pevent) {

  cout<<"StPeCMaker:FillStPeCEvent"<<endl;

  // Set Run and Event Number
  Long_t evno = event->id();
  pevent->setEventNumber(evno);
  Long_t runo = event->id();
  pevent->setEventNumber(runo);

  Int_t   NGlobal=0; 
  Int_t   NPrimaries=0; 
  Int_t   SumQ=0; 
  Float_t SumPt=0.0;
  Float_t SumPx=0.0; 
  Float_t SumPy=0.0;  

  // Get the track nodes
  StSPtrVecTrackNode& exnode = event->trackNodes();
  Int_t nnode=exnode.size();
  for( Int_t in=0; in<nnode; in++ ) {
    UInt_t nprim = exnode[in]->entries(primary);
    UInt_t nglob = exnode[in]->entries(global);
    if( nprim>1 || nglob>1 ){
      cout<<"There could be a problem! nprim= "<<nprim<<"  nglob= "<<nglob<<endl;
    }
    if( nprim==1 ){
      NPrimaries++;
      StTrack *tp = exnode[in]->track(primary);
      float px = tp->geometry()->momentum().x();
      float py = tp->geometry()->momentum().y();
      SumPx = SumPx + px; SumPy = SumPy + py;
      SumQ  = SumQ  + tp->geometry()->charge();
      pevent->addPeCPrimaryTrack(tp);
    }
    if( nprim==0 && nglob==1 ){
      NGlobal++; 
      StTrack *tnp = exnode[in]->track(global);
      pevent->addPeCNonPrimaryTrack(tnp);
    }
  }

  pevent->setPrimMultiplicity(NPrimaries);
  Int_t NTot=NGlobal+NPrimaries;
  pevent->setGlobMultiplicity(NTot);
  pevent->setQTot(SumQ);
  SumPt = sqrt( SumPx*SumPx + SumPy*SumPy );
  pevent->setPT(SumPt);

  StPrimaryVertex* vtx = event->primaryVertex();
  if(!vtx) {
    cout<<"StPeCMaker: There was no primary vertex!"<<endl;
    Float_t fdummy=-9999.9; pevent->setZVertex(fdummy);
    return kStOK;
  }

  Float_t Zv = vtx->position().z();
  pevent->setZVertex(Zv);
  
  return kStOK;
}

Int_t StPeCMaker::FillHistograms(StPeCEvent *pevent) {

  cout<<"StPeCMaker::FillHistograms"<<endl;
  Int_t GlobMult = pevent->globMultiplicity();
  m_hntrk->Fill(1.0*GlobMult);
  Int_t PrimMult = pevent->primMultiplicity();
  m_hnvtxtrk->Fill(1.0*PrimMult);
  Int_t q = pevent->qTot();
  m_hsumq->Fill(q);
  Float_t pt = pevent->pT();
  m_hsumpt->Fill(pt);
  Float_t zv = pevent->zVertex();
  m_hzvert->Fill(zv);

  if( PrimMult == 2 ){
    Float_t mpi = pevent->mInv(pion);
    m_hminvpi->Fill(mpi);
    Float_t mka = pevent->mInv(kaon);
    m_hminvk->Fill(mka);
  }

  Float_t ypi = pevent->yRap(pion);
  m_hrappi->Fill(ypi);
  Float_t yka = pevent->yRap(kaon);
  m_hrapka->Fill(yka);

  return kStOK;
}

Int_t StPeCMaker::ExampleAnalysis(StPeCEvent *pevent) {

  // Below are some examples how to obtain and use the event information
  // through StPeCEvent

  cout<<"StPeCMaker::ExampleAnalysis"<<endl;

  // Some basic event quantities are calculated and stored as data members
  // in StPeCEvent. For example: summed QCharge for primary tracks, pT of
  // the event etc. To retrieve this information do:
  Int_t QEvent = pevent->qTot();
  //  cout<<"Summed charged of the primaries: "<<QEvent<<endl;
  Float_t PtEvent = pevent->pT();
  //  cout<<"Summed event pT: "<<PtEvent<<endl;

  // Other event quantities depend on the particle id, like invariant
  // mass and event rapidity. The member functions for these take 
  // and argument of type StPeCParticle (an enumeration), which currently
  // can be pion, kaon, or proton
  Float_t MInvPionHypo = pevent->mInv(pion);
  //  cout<<"Event invariant mass, assuming pion tracks: "<<MInvPionHypo<<endl;

  // More generally one can obtain the four-momentum of the event 
  // (summed over all primary tracks) by getEvent4Momentum. This is 
  // returned as a StLorentzVectorF
  StLorentzVectorF FourMomentum = pevent->getEvent4Momentum(pion);

  // From this, other event quantities can then easily be calculated. 
  // For example event pT and phi (azimuth):
  Float_t PtAlt = FourMomentum.perp();
  Float_t phi   = FourMomentum.phi();
  //  cout<<"Event pT: "<<PtAlt<<"  azimuthal angle: "<<phi<<endl;

  // Finally it is possible to retrieve the track collections. There
  // are 2 collections: 1 for primary tracks and 1 for all other tracks
  // The collections names are StPeCPrimaryTrackCollection and
  // StPeCNonPrimaryTrackCollection. getEventPeC(Non)PrimaryTrackCollection()
  // returns a pointer to the collection. The elements of the vector
  // are pointers to StTrack (the StEvent track class).
  // The example below loops over all primary tracks and extracts their 
  // momenta

  StPeCPrimaryTrackCollection *pcoll = pevent->getPeCPrimaryTrackCollection();
  Int_t ntrk = pcoll->size();
  //  cout<<"Number of tracks in collection: "<<ntrk<<endl;
  StPeCPrimaryTrackIterator it = pcoll->begin();
  while( it != pcoll->end() ){
    StTrack *ttp = *it;
    StTrackGeometry *geo = ttp->geometry();
    Float_t px = geo->momentum().x();
    Float_t py = geo->momentum().y();
    Float_t pz = geo->momentum().z();
    it++;
  }

  return kStOK;
}


void StPeCMaker::Clear(Option_t *opt) {
  StMaker::Clear();
}


Int_t StPeCMaker::Finish() {
  m_outfile->Write();
  m_outfile->Close();
  return kStOK;
}














