// $Id: StPeCMaker.cxx,v 1.10 2000/12/13 00:08:59 akio Exp $
// $Log: StPeCMaker.cxx,v $
// Revision 1.10  2000/12/13 00:08:59  akio
// Added trigger sim and histograms
//
// Revision 1.9  2000/04/21 19:09:49  nystrand
// Update StPeCPair class, new histograms
//
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
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#include "StL0Trigger.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StMwcTriggerDetector.h"
#include "StVpdTriggerDetector.h"
#include "StZdcTriggerDetector.h"

static const char rcsid[] = "$Id: StPeCMaker.cxx,v 1.10 2000/12/13 00:08:59 akio Exp $";

int l0sim(StEvent*, TH1F*);

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
  m_hminvpi   = new TH1F("hminvpi","2-Track Evts. Minv pions",50,0.2,1.5);
  m_hminvk    = new TH1F("hminvk","2-Track Evts. Minv kaons",50,0.8,2.0);
  m_hrappi    = new TH1F("hrappi","Evt. Rapidity (pions)",50,-5.0,5.0);
  m_hrapka    = new TH1F("hrapka","Evt. Rapidity (kaons)",50,-5.0,5.0);
  m_hopnangle = new TH1F("hopnangle","Opening angle of the pairs (rad)",100,0.0,3.15);
  m_hcostheta = new TH1F("hcostheta","Cos(Theta*) (for pairs of pions) ",120,-0.1,1.1); 
  m_hdedx     = new TH2F("hdedx","P (GeV) vs dE/dx (*10**6)",200,0.0,2.0,100,0.0,20.0);
  m_hdedxpos  = new TH2F("hdedxpos","P (GeV) vs dE/dx (*10**6) +",200,0.0,2.0,100,0.0,20.0);
  m_hdedxneg  = new TH2F("hdedxneg","P (GeV) vs dE/dx (*10**6) -",200,0.0,2.0,100,0.0,20.0);
  m_hdedx1    = new TH1F("hdedx1","dE/dx (*10**6)",100,0.0,10.0);
  m_ctbsingle = new TH1F("ctbsingle","CTB single",100,0.0,100.0);
  m_ctbsum    = new TH1F("ctbsum","CTB sum",100,0.0,100.0);
  m_ctbsumped = new TH1F("ctbsumped","CTB sum pedestal",50,0.0,50.0);
  m_ctbtrg    = new TH1F("ctbtrg","CTB trigger",60,0.0,60.0);
  m_zdcwest   = new TH1F("zdcwest","zdc adc west",100,0.0,100.0);
  m_zdceast   = new TH1F("zdceast","zdc adc east",100,0.0,100.0);
  m_zdcsum    = new TH1F("zdcsum","zdc adc sum",100,0.0,100.0);
  m_ctbvstrk  = new TH2F("ctbvstrk","ctbsum vs # of global track",100,0.0,100.0,100,0.0,100.0);
  m_ctbslat   = new TH2F("ctbslat","ctbadc vs slat #",240,0.5,240.5,39,1.0,40.0);

  m_hxvert      = new TH1F("hxvert","X Primary Vertex (cm)",50,-5.0,5.0);
  m_hyvert      = new TH1F("hyvert","Y Primary Vertex (cm)",50,-5.0,5.0);
  m_hxyvert     = new TH2F("hxyvert","XY Primary Vertex (cm)",50,-5.0,5.0,50,-5.0,5.0);
  m_hzvert      = new TH1F("hzvert","Z Primary Vertex (cm)",100,-500.0,500.0);

  m_hnpair      = new TH1F("hnpair","Number of pair",20,0.0,20.0);
  m_hpairsumq   = new TH1F("hpsumq","Pair sum Q",11,-5.5,5.5);
  m_hpairoa     = new TH1F("hpoa","Opening angle of the pairs (rad)",100,0.0,3.15);
  m_hpaircostpi = new TH1F("hpcosthetap","Cos(Theta*) (for pairs of pions) ",120,-0.1,1.1);
  m_hpaircostk  = new TH1F("hpcosthetak","Cos(Theta*) (for pairs of kaons) ",120,-0.1,1.1);
  m_hpairsumpt  = new TH1F("hpsumpt","Pair sum pT",50,0.0,1.0);
  m_hpairminvpi = new TH1F("hpminvpi","Pair Minv pions",50,0.2,1.5);
  m_hpairminvk  = new TH1F("hpminvk","Pair Minv kaons",50,0.8,2.0);
  m_hpairdedx   = new TH2F("hpdedx","P (GeV) vs dE/dx (*10**6)",200,0.0,2.0,100,0.0,20.0);

  m_hcpairsumq   = new TH1F("hcpsumq","Pair sum Q (rho0 mass)",11,-5.5,5.5);
  m_hlpairsumq   = new TH1F("hlpsumq","Pair sum Q (low mass) ",11,-5.5,5.5);

  m_hcpairsumpt  = new TH1F("hcpsumpt","Pair sum pT",50,0.0,1.0);
  m_hcpairminvpi = new TH1F("hcpminvpi","Pair Minv pions",50,0.2,1.5);
  m_hcpairminvk  = new TH1F("hcpminvk","Pair Minv kaons",50,0.8,2.0);
  m_hcpairdedx   = new TH2F("hpcdedx","P (GeV) vs dE/dx (*10**6)",200,0.0,2.0,100,0.0,20.0);

  m_hrhoptall    = new TH1F("hrhoptall","Rho pt",50,0.0,1.00);

  m_hmass        = new TH1F("hmass","Pair Minv pions (after pt cut)",50,0.2,1.5);
  m_hmasszdc     = new TH2F("masszdcsum","mass vs zdc ",50,0.2,1.5,100,0.0,100.0);

  m_hrhonpair    = new TH1F("hrhonpair","Number of pair",20,0.0,20.0);
  m_hrhoxvert    = new TH1F("hrhoxvert","X rho0 Vertex (cm)",50,-5.0,5.0);
  m_hrhoyvert    = new TH1F("hrhoyvert","Y rho0 Vertex (cm)",50,-5.0,5.0);
  m_hrhozvert    = new TH1F("hrhozvert","Z rho0 Vertex (cm)",100,-200.0,200.0);
  m_hrhocost     = new TH1F("hrhocost","Cos(Theta*) for rho",50,-0.1,1.1);
  m_hrhopt       = new TH1F("hrhopt","Rho0 pT",25,0.0,0.25);
  m_hrhodedx     = new TH2F("hrhodedx","P (GeV) vs dE/dx (*10**6)",200,0.0,2.0,100,0.0,20.0);
  m_hrhodedx1    = new TH1F("hrhodedx1","dE/dx (*10**6) rho0",100,0.0,5.0);
  m_hrhorapidity = new TH1F("hrhorapidity","Rho0 rapidity",40,-2.0,2.0);
  m_hrhodndpt2   = new TH1F("hrhodndpt2","Rho0 dN/dpt^2",25,.0,0.25);
  m_hrhozdcsum   = new TH1F("rhozdcsum","zdc sum for rho0",100,0.0,100.0);

  m_hlowmasspt   = new TH1F("hlowmasspt","mass<Rho0 pT",25,0.0,0.25);
  m_hlowmasszdcsum= new TH1F("hlowmasszdcsum","zdc sum for low mass",100,0.0,100.0);
  m_hlowmassdedx1= new TH1F("hlowmassdedx1","dE/dx (*10**6) low mass",100,0.0,5.0);

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

  // trigger simulations
  int trig = triggerSim(event);

  // Do this way since call to event->summary->numberOfTracks() crashes
  StSPtrVecTrackNode& tempn = event->trackNodes();
  Int_t NTracks=tempn.size();
  if( NTracks > 15 ){
    cout<<"StPeCMaker: Number of good tracks: "<<NTracks<<endl;
    cout<<"Not a peripheral event (NTracks>15)"<<endl;
    //    return kStOK;
    return kStErr; // to skip i/o for non pec evemts
  }
  if( NTracks <= 0 ){
    cout<<"StPeCMaker: Event has no tracks!"<<endl;
    //    return kStOK;
    return kStErr; // to skip i/o for non pec evemts
  }
  cout<<"StPeCMaker: This may be a peripheral event (0<NTracks<15)"<<endl;

  // Instantiate StPeCEvent
  StPeCEvent *pevent = new StPeCEvent;

  // Count the events with 0<Ntrk<15
  m_hstat->Fill(1.0);

  // Fill StPeCEvent, check if primary vertex found
  Int_t ireturn;
  ireturn = FillStPeCEvent(event,pevent);
  if(ireturn>kStWarn) return kStErr; 

  // Fill the PeC histograms using StPeCEvent
  ireturn = FillHistograms(pevent);
  
  // Example of routine how to use StPeCEvent for analysis
  ireturn = ExampleAnalysis(pevent);

  // Example of further cuts, returning kStErr for rejecting event
  ireturn = Cuts(event, pevent);
  delete event; delete pevent;
  return ireturn;
}


Int_t StPeCMaker::FillStPeCEvent(StEvent *event, StPeCEvent *pevent){

  cout<<"StPeCMaker:FillStPeCEvent"<<endl;

  // Set Run and Event Number
  Long_t evno = event->id();
  pevent->setEventNumber(evno);
  Long_t runo = event->runId();
  pevent->setEventNumber(runo);

  //get trigger info
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  StCtbTriggerDetector& ctb = trg->ctb();
  StMwcTriggerDetector& mwc = trg->mwc();
  StVpdTriggerDetector& vpd = trg->vpd();
  StZdcTriggerDetector& zdc = trg->zdc();

  Int_t   NGlobal=0; 
  Int_t   NPrimaries=0; 
  Int_t   SumQ=0; 
  Float_t SumPt=0.0;
  Float_t SumPx=0.0; 
  Float_t SumPy=0.0;  
#ifndef __CINT__
  vector<StTrack*> ThePrimaries;
#endif /* __CIN__ */

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
      // Store the Primaries in a vector for formation of pairs
#ifndef __CINT__
      ThePrimaries.push_back(tp);
#endif /* __CINT__ */
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
    //    return kStOK;
    return kStErr;
  }
  Float_t Zv = vtx->position().z();
  pevent->setZVertex(Zv);
  m_hxvert->Fill(vtx->position().x());
  m_hyvert->Fill(vtx->position().y());
  m_hxyvert->Fill(vtx->position().x(),vtx->position().y());
  cout << "StPeCMaker : primary vertex " << vtx->position().x() << " " 
       << vtx->position().y() << " " << vtx->position().z() << endl;

  // Fill the pairs into StPeCPair
#ifndef __CINT__
  Int_t NVect = ThePrimaries.size();
  if ( NPrimaries != NVect ){
    cout<<"StPeCMaker: Warning pair vector might be incorrect!"<<endl;}
  for ( Int_t i1 = 0; i1<NPrimaries-1; i1++ ) {
    for( Int_t i2 = i1+1; i2<NPrimaries; i2++ ) {
      StTrack *trk1 = ThePrimaries[i1];
      StTrack *trk2 = ThePrimaries[i2];
      StPeCPair *thepair = new StPeCPair(trk1,trk2);
      pevent->addPeCPair(thepair);
    }
  }  
  StPeCPairCollection *pairs= pevent->getPeCPairCollection();
  StPeCPairIterator itr = pairs->begin();
  m_hnpair->Fill(pairs->size());
  while( itr != pairs->end() ){
    StPeCPair *pair = *itr;
    StTrackGeometry *trk1 = pair->getTrack1()->geometry();
    StTrackGeometry *trk2 = pair->getTrack2()->geometry();
    StSPtrVecTrackPidTraits& traits1 = pair->getTrack1()->pidTraits();
    StSPtrVecTrackPidTraits& traits2 = pair->getTrack1()->pidTraits();
    StDedxPidTraits *dedx1, *dedx2;
    Int_t NTraits = traits1.size();
    for( Int_t i=0; i<NTraits; i++) {
      if ( traits1[i]->detector() == kTpcId ){
	dedx1 = dynamic_cast<StDedxPidTraits*>(traits1[i]);
        if ( dedx1 && dedx1->method() == kTruncatedMeanIdentifier )break;
      }
    }
    NTraits = traits2.size();
    for(i=0; i<NTraits; i++) {
      if ( traits2[i]->detector() == kTpcId ){
	dedx2 = dynamic_cast<StDedxPidTraits*>(traits2[i]);
        if ( dedx2 && dedx2->method() == kTruncatedMeanIdentifier )break;
      }
    }
    cout << "StPeCMaker : Pair : " 
	 << "  sumQ = " << pair->sumCharge()
	 << "  sumPt = " << pair->sumPt()
	 << "  mInv = " << pair->mInv(pion)
	 << "  opening angle = " << pair->openingAngle()
	 << "  cos(theata*) = " << pair->cosThetaStar(pion) << endl;

    m_hpairsumq->Fill(pair->sumCharge());
    m_hpairoa->Fill(pair->openingAngle());
    m_hpaircostpi->Fill(pair->cosThetaStar(pion));
    m_hpaircostk->Fill(pair->cosThetaStar(kaon));
    m_hpairsumpt->Fill(pair->sumPt());
    m_hpairminvpi->Fill(pair->mInv(pion));
    m_hpairminvk->Fill(pair->mInv(kaon));
    m_hpairdedx->Fill(pair->mInv(kaon));    
    m_hpairdedx->Fill(trk1->momentum().mag(),1000000.0*dedx1->mean());
    m_hpairdedx->Fill(trk2->momentum().mag(),1000000.0*dedx2->mean());  
    itr++;
  }
}

Int_t StPeCMaker::Cuts(StEvent *event, StPeCEvent *pevent){
  int flag = 0;

  //get trigger info
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  StCtbTriggerDetector& ctb = trg->ctb();
  StMwcTriggerDetector& mwc = trg->mwc();
  StVpdTriggerDetector& vpd = trg->vpd();
  StZdcTriggerDetector& zdc = trg->zdc();

  //get vertex info
  StPrimaryVertex* vtx = event->primaryVertex();

  StPeCPairCollection *pairs= pevent->getPeCPairCollection();
  StPeCPairIterator itr = pairs->begin();

  //fill histograms
  while( itr != pairs->end() ){
    StPeCPair *pair = *itr;
    StTrackGeometry *trk1 = pair->getTrack1()->geometry();
    StTrackGeometry *trk2 = pair->getTrack2()->geometry();
    StSPtrVecTrackPidTraits& traits1 = pair->getTrack1()->pidTraits();
    StSPtrVecTrackPidTraits& traits2 = pair->getTrack1()->pidTraits();
    StDedxPidTraits *dedx1, *dedx2;
    Int_t NTraits = traits1.size();
    for( Int_t i=0; i<NTraits; i++) {
      if ( traits1[i]->detector() == kTpcId ){
	dedx1 = dynamic_cast<StDedxPidTraits*>(traits1[i]);
        if ( dedx1 && dedx1->method() == kTruncatedMeanIdentifier )break;
      }
    }
    NTraits = traits2.size();
    for(i=0; i<NTraits; i++) {
      if ( traits2[i]->detector() == kTpcId ){
	dedx2 = dynamic_cast<StDedxPidTraits*>(traits2[i]);
        if ( dedx2 && dedx2->method() == kTruncatedMeanIdentifier )break;
      }
    }

    if(
       -1.5<vtx->position().x() && vtx->position().x()<2.5 &&
       -1.5<vtx->position().y() && vtx->position().y()<2.5 &&
       -200.0<vtx->position().z() && vtx->position().z()<200.0 &&
       pair->openingAngle()<3.0 &&
       pairs->size()==1){
      
      // different mass cut
      float mmin = 0.47, mmax = 1.07;
      //float mmin = 0.62, mmax = 0.92;

      if(pair->sumPt()<0.3){
	if(mmin<pair->mInv(pion) && pair->mInv(pion)<mmax){
	  m_hcpairsumq->Fill(pair->sumCharge());
	}else if(mmin>pair->mInv(pion)){
	  m_hlpairsumq->Fill(pair->sumCharge());
	}
      }
      
      if(pair->sumCharge()==0){
	m_hcpairsumpt->Fill(pair->sumPt());
	m_hcpairminvpi->Fill(pair->mInv(pion));
	m_hcpairminvk->Fill(pair->mInv(kaon));
	m_hcpairdedx->Fill(trk1->momentum().mag(),1000000.0*dedx1->mean());
	m_hcpairdedx->Fill(trk2->momentum().mag(),1000000.0*dedx2->mean());
	
	//to make smaller dst with tight cut
	if(pair->sumPt()<0.3) flag=1;
		
	if(mmin<pair->mInv(pion) && pair->mInv(pion)<mmax){
	  m_hrhoptall->Fill(pair->sumPt());
	}
	
	if(pair->sumPt()<0.3){
	  m_hmass->Fill(pair->mInv(pion));
	  if(&zdc){
	    m_hmasszdc->Fill(pair->mInv(pion),zdc.adcSum());
	  }
	  
	  if(mmin<pair->mInv(pion) && pair->mInv(pion)<mmax){
	    m_hrhonpair->Fill(pairs->size());
	    m_hrhoxvert->Fill(vtx->position().x());
	    m_hrhoyvert->Fill(vtx->position().y());
	    m_hrhozvert->Fill(vtx->position().z());
	    m_hrhocost->Fill(pair->cosThetaStar(pion));
	    m_hrhopt->Fill(pair->sumPt());
	    m_hrhodedx->Fill(trk1->momentum().mag(),1000000.0*dedx1->mean());
	    m_hrhodedx->Fill(trk2->momentum().mag(),1000000.0*dedx2->mean());
	    m_hrhodedx1->Fill(1000000.0*dedx1->mean());
	    m_hrhodedx1->Fill(1000000.0*dedx2->mean());
	    
	    StLorentzVectorF p = pair->getPair4Momentum(pion);	    
	    m_hrhorapidity->Fill(p.rapidity());
	    
	    m_hrhodndpt2->Fill(pair->sumPt(),1.0/pair->sumPt());
	    if(&zdc){
	      m_hrhozdcsum->Fill(zdc.adcSum());
	    }
	  }
	  
	  if(mmin>pair->mInv(pion)){
	    m_hlowmasspt->Fill(pair->sumPt());
	    m_hlowmassdedx1->Fill(1000000.0*dedx1->mean());
	    m_hlowmassdedx1->Fill(1000000.0*dedx2->mean());
	    if(&zdc){
	      m_hlowmasszdcsum->Fill(zdc.adcSum());
	    }
	  }	 
	}
      }
    }  
    itr++;
  }
  
  //Return kStErr if this event shouldn't go into uDST
  if(pairs->size()!=1){
    cout<<"StPeCMaker: Number of pairs does not match"<<endl;
    return kStErr;
  }    
  if(flag==0) {
    cout<<"StPeCMaker: No sumPt<0.3GeV pair"<<endl;
    // return kStErr;
  }
  
#endif /* __CINT__ */

  return kStOK;
}

Int_t StPeCMaker::FillHistograms(StPeCEvent *pevent) {

  cout<<"StPeCMaker:FillHistograms"<<endl;
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

  StPeCPairCollection *pair = pevent->getPeCPairCollection();
  StPeCPairIterator itp = pair->begin();
  while( itp != pair->end() ){
    StPeCPair *pp = *itp;
    Float_t theta_open = pp->openingAngle();
    Float_t costheta   = pp->cosThetaStar(pion);
    m_hopnangle->Fill(theta_open);
    m_hcostheta->Fill(costheta);
    itp++;
  }

  // Loop over Tracks to extract TPC dE/dx (TruncatedMean)
  StPeCPrimaryTrackCollection *pprimv = pevent->getPeCPrimaryTrackCollection();
  StPeCPrimaryTrackIterator pprimit = pprimv->begin();
  while( pprimit != pprimv->end() ){
    StTrack *pprimt = *pprimit;
    StTrackGeometry *geo = pprimt->geometry();
    Float_t ptot = geo->momentum().mag();
    StSPtrVecTrackPidTraits& traits = pprimt->pidTraits();
    StDedxPidTraits *dedx;
    Int_t NTraits = traits.size();
    for( Int_t i=0; i<NTraits; i++) {
      if ( traits[i]->detector() == kTpcId ){
	dedx = dynamic_cast<StDedxPidTraits*>(traits[i]);
        if ( dedx && dedx->method() == kTruncatedMeanIdentifier )break;
      }
    }
    // dedx now contains the dedx according to the TruncatedMean
    Float_t TrunkMean = 1000000.0*dedx->mean();
    m_hdedx->Fill(ptot,TrunkMean);
    m_hdedx1->Fill(TrunkMean);
    Int_t QCharge = geo->charge();
    if( QCharge > 0 )m_hdedxpos->Fill(ptot,TrunkMean);
    if( QCharge < 0 )m_hdedxneg->Fill(ptot,TrunkMean);
    pprimit++;
  }
  return kStOK;
}

Int_t StPeCMaker::ExampleAnalysis(StPeCEvent *pevent) {

  // Below are some examples how to obtain and use the event information
  // through StPeCEvent

  cout<<"StPeCMaker:ExampleAnalysis"<<endl;

  // Some basic event quantities are calculated and stored as data members
  // in StPeCEvent. For example: summed QCharge for primary tracks, pT of
  // the event etc. To retrieve this information do:
  Int_t QEvent = pevent->qTot();
  cout << "StPeCMaker: q Total = " << QEvent << endl;
  Float_t PtEvent = pevent->pT();
  cout << "StPeCMaker: pT Total = " << PtEvent << endl;

  // Other event quantities depend on the particle id, like invariant
  // mass and event rapidity. The member functions for these take 
  // an argument of type StPeCParticle (an enumeration), which currently
  // can be pion, kaon, proton, electron or muon
  Float_t MInvPionHypo = pevent->mInv(pion);
  cout << "StPeCMaker: Minv(pion) = " << MInvPionHypo << endl;

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

  // For an example of how to use the StPeCPair class, check the FillHistograms
  // member function.

  return kStOK;
}

Int_t StPeCMaker::triggerSim(StEvent *event){
  int i,j,k;
  
  StL0Trigger* l0 = event->l0Trigger();
  StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
  StCtbTriggerDetector& ctb = trg->ctb();
  StMwcTriggerDetector& mwc = trg->mwc();
  StVpdTriggerDetector& vpd = trg->vpd();
  StZdcTriggerDetector& zdc = trg->zdc();

  if(l0){
    cout << "L0 mwcCtbMultiplicity = " << l0->mwcCtbMultiplicity() << endl;
  }
  if(&zdc){
    cout << "ZDC sum " << zdc.adcSum() << endl;
    cout << "ZDC sum west " << zdc.adcSum(west) << endl;
    cout << "ZDC sum east " << zdc.adcSum(east) << endl;
    m_zdcwest->Fill(zdc.adcSum(west));
    m_zdceast->Fill(zdc.adcSum(east));
    m_zdcsum->Fill(zdc.adcSum());
  }
  if(&mwc){
    float sum = 0.0;
    for(i=0; i<mwc.numberOfSectors(); i++){
      for(j=0; j<mwc.numberOfSubSectors(); j++){
	sum += mwc.mips(i,j,0);
      }
    }
    cout << "mwc mips " << sum << endl;
  }
  if(&ctb){
    float ctbsum=0.0;
    for(i=0; i<120; i++){
      for(j=0; j<2; j++){
	ctbsum += ctb.mips(i,j,0);
	m_ctbsingle->Fill(ctb.mips(i,j,0));
	m_ctbslat->Fill(i+1+120*j, ctb.mips(i,j,0));
      }
    }
    m_ctbsum->Fill(ctbsum);
    StSPtrVecTrackNode& trknode = event->trackNodes();
    int nnode=trknode.size();
    m_ctbvstrk->Fill(ctbsum,nnode);
    cout << "CTB MIP total sum = " << ctbsum << endl;
  }

  // L0 simulator
  int res =  l0sim(event, m_ctbsum);
  return res;
}

// void StPeCMaker::Clear(Option_t *opt) {
// StMaker::Clear();
// }
  
Int_t StPeCMaker::Finish() {
  m_outfile->Write();
  m_outfile->Close();
  StMaker::Finish();
  return kStOK;
}


