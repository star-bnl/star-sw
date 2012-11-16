/**********************************************************************
 *
 * $Id: StEStructQAHists.cxx,v 1.11 2012/11/16 21:19:07 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  base class for QA histogramming
 *
 ***********************************************************************/
#include "StEStructQAHists.h"

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"
#include "TH1F.h"
#include "TH1D.h"
#include "TH2F.h"
#include "TFile.h"

ClassImp(StEStructQAHists)

StEStructQAHists::StEStructQAHists(int itype){ 
  mEType=itype;
  mCents[0]=mCents[1]=NULL;
  mptCents[0]=mptCents[1]=mptCents[2]=NULL;
  mRefMult=0;
  mTotMult=mPosMult=mNegMult=0;
  mTotMult4=mPosMult4=mNegMult4=0;
  mntBins = 0;
  mhasAIndex=false;
  initHistograms();
};


StEStructQAHists::~StEStructQAHists(){ };

//---------------------------------------------------------
//
//   add your stuff below
//
//--------------------------------------------------------
void StEStructQAHists::initHistograms(){ 
  
  initBaseHistograms();
  /* <-- add your analysis dependent set here --> */

};

void StEStructQAHists::fillHistograms(StEStructEvent* event, StEStructEventReader* reader){
  fillBaseHistograms(event,reader);
};

void StEStructQAHists::writeHistograms(TFile * tf){

  writeBaseHistograms(tf);
  if(!mhasAIndex) writeTrackHistograms(tf);

  /* --- add your analysis dependent stuff here  --- */
};


// 
//
//  ----   Below are common to all  -----
//
//


void StEStructQAHists::initBaseHistograms(){ 

  StEStructCentrality* cent = StEStructCentrality::Instance();
  
  int mbNBins = cent->numCentralities();
  if(mbNBins>0){
     mCents[0] = new TH1D("cenClass","cenClass",mbNBins,-0.5,mbNBins-0.5);
     mCents[1] = new TH1D("centralityDefs","centralityDefs",mbNBins,0.5,mbNBins+0.5);
  }

  // fill the centrality mapping
  for (int i=0;i<mbNBins;i++) mCents[1]->Fill(i,cent->centralityLimit(i));

  int mbNPtBins = cent->numPtCentralities();
  if(mbNPtBins>0){
    mptCents[0]= new TH1D("ptCenClass","ptCenClass",mbNPtBins,-0.5,mbNPtBins-0.5);
    mptCents[1]= new TH1D("centralityPtDefs","centralityPtDefs",mbNPtBins,0.5,mbNPtBins+0.5);
  }
  for (int i=0;i<mbNPtBins;i++) mptCents[1]->Fill(i,cent->ptCentralityLimit(i));


  int mbNPts    = cent->numPts();
  if(mbNPts>0){
    mptCents[2] = new TH1D("ptRanges","ptRanges",mbNPts,-0.5,mbNPts+0.5);
  }
  for (int i=0;i<mbNPts;i++) mptCents[i]->Fill(i,cent->ptLimit(i)); 

  mRefMult = new TH1D("refMult","refMult for comparison",2500,-0.5,2500-0.5);
  mTotMult = new TH1D("totalMultiplicity","totalMultiplicity",2500,-0.5,2500-0.5);
  mPosMult = new TH1D("positiveMultiplicity","positiveMultiplicity",1500,-0.5,1500-0.5);
  mNegMult = new TH1D("negativeMultiplicity","negativeMultiplicity",1500,-0.5,1500-0.5);
  mTotMult4 = new TH1D("nTotOneQuarter","nTotOneQuarter",100,0.0,7.0);
  mPosMult4 = new TH1D("nPosOneQuarter","nPosOneQuarter",100,0.0,7.0);
  mNegMult4 = new TH1D("nNegOneQuarter","nNegOneQuarter",100,0.0,7.0);

  if(mEType==1) {  // AA monte carlo generator
     
     aaGenImpact = new TH1D("impact","impact",100,0.0,20.0);
     aaGen[0] = new TH2F("binary","binary",500,0.5,2000.5,50,0.0,20.0);
     aaGen[1] = new TH2F("participant","participant",500,0.5,500.5,50,0.0,20.0);

     aaGenBin  = new TH1D*[mbNBins-1];
     aaGenPart = new TH1D*[mbNBins-1];
     for(int i=0;i<mbNBins-1;i++){
       
       TString bName("binary_");      bName += i;
       TString pName("participant_"); pName += i;
       aaGenBin[i]  = new TH1D(bName.Data(),bName.Data(),2000,0.5,2000.5);
       aaGenPart[i] = new TH1D(pName.Data(),pName.Data(),2000,0.5,2000.5);

     }
  }            

  if(mEType==2){  //pp event generator ...

    ppELines = new TH1F*[mbNBins-1];
    ppALines = new TH1F*[mbNBins-1];

    for(int i=0;i<mbNBins-1;i++){

      TString pName("partonLines_"); pName+=i;
      ppELines[i] = new TH1F(pName.Data(),pName.Data(),1000,0.5,1000.5);
      TString aName("allPartonLines_"); aName+=i;
      ppALines[i] = new TH1F(aName.Data(),aName.Data(),200,0.,4000.);
    }
  }
      

};

void StEStructQAHists::fillBaseHistograms(StEStructEvent* event, StEStructEventReader* reader){

  if(!event) return;

  
  StEStructCentrality* cent = StEStructCentrality::Instance();
  int ic = cent->centrality(event->Centrality());
  mCents[0]->Fill(ic);
  if(mptCents[0])mptCents[0]->Fill(ic);
  mRefMult->Fill(event->RefMult());
  mTotMult->Fill(event->Ntrack());
  mPosMult->Fill(event->Npos());
  mNegMult->Fill(event->Nneg());
  mTotMult4->Fill(pow(event->Ntrack(),0.25));
  mPosMult4->Fill(pow(event->Npos(),0.25));
  mNegMult4->Fill(pow(event->Nneg(),0.25));

  if(mEType==1){
    if(aaGenImpact)aaGenImpact->Fill(reader->getImpact());
    if(aaGen[0])aaGen[0]->Fill(reader->getBinary(),reader->getImpact());
    if(aaGen[1])aaGen[1]->Fill(reader->getParticipants(),reader->getImpact());
    if(ic>=0){
      if(aaGenBin[ic]) aaGenBin[ic]->Fill(reader->getBinary());
      if(aaGenPart[ic])aaGenPart[ic]->Fill(reader->getParticipants());
    }
  } else if(mEType==2){
    if(ic>=0){
      ppELines[ic]->Fill(reader->getNPartonic());
      ppALines[ic]->Fill(reader->getNPartonic());
    }
  }
  

};

void StEStructQAHists::writeBaseHistograms(TFile* tf){
  tf->cd();

  for(int i=0;i<2;i++)if(mCents[i])mCents[i]->Write();
  for(int i=0;i<3;i++)if(mptCents[i])mptCents[i]->Write();
  mRefMult->Write();
  mTotMult->Write();
  mPosMult->Write();
  mNegMult->Write();
  mTotMult4->Write();
  mPosMult4->Write();
  mNegMult4->Write();
  if(mEType==1){
    if(aaGenImpact)aaGenImpact->Write();
    for(int i=0;i<2;i++)if(aaGen[i])aaGen[i]->Write();
    for(int i=0;i<StEStructCentrality::Instance()->numCentralities()-1;i++){
      if(aaGenBin[i])aaGenBin[i]->Write();
      if(aaGenPart[i])aaGenPart[i]->Write();
    }
  } else if(mEType==2){
    for(int i=0;i<StEStructCentrality::Instance()->numCentralities()-1;i++){
      if(ppELines[i])ppELines[i]->Write();
      if(ppALines[i])ppALines[i]->Write();
    }
  }

};

void StEStructQAHists::initTrackHistograms(int numBins, int aIndex){

  if(mntBins>0)return; // already been here in init

  mntBins = numBins;
  int qbins = 2*numBins;

  mHEta      = new TH1F*[qbins];
  mHPhi      = new TH1F*[qbins];
  mHPt       = new TH1F*[qbins];
  mHYt       = new TH1F*[qbins];
  mHMass     = new TH1F*[qbins];
  mHdEdxPtot = new TH2F*[numBins];
  mHToFPtot  = new TH2F*[numBins];
  mHEtaPt    = new TH2F*[qbins];

  int nall = 100;
  int nMass = 160;
  float xetamin = -2.0;
  float xetamax = 2.0;
  float xphimin = -M_PI;
  float xphimax = M_PI;
  float xptmin  = 0.;
  float xptmax  = 6.0;
  float xytmin  = 0.5;
  float xytmax  = 5.0;
  float xmassmin  = 0.0;
  float xmassmax  = 4.0;
  
  int nx = 101;
  int ny = 101;
  float xpmin = -2.5;
  float xpmax =  2.5;
  float ydmin =  0.;
  float ydmax = 15.0e-06;
  float ytofmin = 0.;
  float ytofmax = 1.5;

  TString haIndex;
  if(aIndex>=0){
    mhasAIndex=true;
    haIndex+="_A";
    haIndex+=aIndex;
    haIndex+="_";
  }

  for(int i=0; i<numBins;i++){

    TString heta("Eta"); 
    if(mhasAIndex)heta+=haIndex.Data();
    if(numBins>1)heta+=i;
    TString hpeta("Qp"); hpeta+=heta.Data();
    mHEta[i]=new TH1F(hpeta.Data(),hpeta.Data(),nall,xetamin,xetamax);
    TString hmeta("Qm"); hmeta+=heta.Data();
    mHEta[i+numBins]=new TH1F(hmeta.Data(),hmeta.Data(),nall,xetamin,xetamax);

    TString hphi("Phi");
    if(mhasAIndex)hphi+=haIndex.Data();
    if(numBins>1)hphi+=i;
    TString hpphi("Qp"); hpphi+=hphi.Data();
    mHPhi[i]=new TH1F(hpphi.Data(),hpphi.Data(),nall,xphimin,xphimax);
    TString hmphi("Qm"); hmphi+=hphi.Data();
    mHPhi[i+numBins]=new TH1F(hmphi.Data(),hmphi.Data(),nall,xphimin,xphimax);

    TString hpt("Pt");
    if(mhasAIndex)hpt+=haIndex.Data();
    if(numBins>1)hpt+=i;
    TString hppt("Qp"); hppt+=hpt.Data();
    mHPt[i]=new TH1F(hppt.Data(),hppt.Data(),nall,xptmin,xptmax);
    TString hmpt("Qm"); hmpt+=hpt.Data();
    mHPt[i+numBins]=new TH1F(hmpt.Data(),hmpt.Data(),nall,xptmin,xptmax);

    TString hyt("Yt");
    if(mhasAIndex)hyt+=haIndex.Data();
    if(numBins>1)hyt+=i;
    TString hpyt("Qp"); hpyt+=hyt.Data();
    mHYt[i]=new TH1F(hpyt.Data(),hpyt.Data(),nall,xytmin,xytmax);
    TString hmyt("Qm"); hmyt+=hyt.Data();
    mHYt[i+numBins]=new TH1F(hmyt.Data(),hmyt.Data(),nall,xytmin,xytmax);

    TString hdedxP("dEdx_P");
    if(mhasAIndex)hdedxP+=haIndex.Data();
    if(numBins>1)hdedxP+=i;   
    mHdEdxPtot[i] = new TH2F(hdedxP.Data(),hdedxP.Data(),nx,xpmin,xpmax,ny,ydmin,ydmax);

    TString hmass("Mass");
    if(mhasAIndex)hmass+=haIndex.Data();
    if(numBins>1)hmass+=i;
    TString hpmass("Qp"); hpmass+=hmass.Data();
    mHMass[i]=new TH1F(hpmass.Data(),hpmass.Data(),nMass,xmassmin,xmassmax);
    TString hmmass("Qm"); hmmass+=hmass.Data();
    mHMass[i+numBins]=new TH1F(hmmass.Data(),hmmass.Data(),nMass,xmassmin,xmassmax);

    TString htofP("mass_P");
    if(mhasAIndex)htofP+=haIndex.Data();
    if(numBins>1)htofP+=i;   
    mHToFPtot[i] = new TH2F(htofP.Data(),htofP.Data(),nx,xpmin,xpmax,ny,ytofmin,ytofmax);

    TString hetapt("EtaPt"); 
    if(mhasAIndex)hetapt+=haIndex.Data();
    if(numBins>1)hetapt+=i;
    TString hpetapt("Qp"); hpetapt+=hetapt.Data();
    mHEtaPt[i]=new TH2F(hpetapt.Data(),hpetapt.Data(),20,-1.0,1.0,30,0.1,3.1);
    TString hmetapt("Qm"); hmetapt+=hetapt.Data();
    mHEtaPt[i+numBins]=new TH2F(hmetapt.Data(),hmetapt.Data(),20,-1.0,1.0,30,0.1,3.1);
  }

};


void StEStructQAHists::fillTrackHistograms(StEStructTrack* t, int ib) {

    if (mntBins==0 || ib>mntBins) {
        return;
    }
    int i = ib;
    if (t->Charge()<0) {
        mHdEdxPtot[i]->Fill(-t->Ptot(),t->Dedx());
        mHToFPtot[i]->Fill(-t->Ptot(),t->Mass());
        i += mntBins;
    } else {
        mHdEdxPtot[i]->Fill(t->Ptot(),t->Dedx());
        mHToFPtot[i]->Fill(t->Ptot(),t->Mass());
    }

    mHEta[i]->Fill(t->Eta());
    mHPhi[i]->Fill(t->Phi());
    mHPt[i]->Fill(t->Pt());
    mHYt[i]->Fill(t->Yt());
    mHEtaPt[i]->Fill(t->Eta(),t->Pt());
    mHMass[i]->Fill(t->Mass());

};


void StEStructQAHists::writeTrackHistograms(TFile* tf){

  if(!tf || mntBins==0)return;
  tf->cd();

  for(int i=0;i<2*mntBins;i++){
    mHEta[i]->Write();
    mHPhi[i]->Write();
    mHPt[i]->Write();
    mHYt[i]->Write();
    mHEtaPt[i]->Write();
    mHMass[i]->Write();
  }
  for(int i=0;i<mntBins;i++){
    mHdEdxPtot[i]->Write();
    mHToFPtot[i]->Write();
  }

};

// Maybe implement these.
// Probably want to control via a doPairHistogram flag or something.
//void StEStructQAHists::initPairHistograms(int numBins, int aIndex){
//};
//void StEStructQAHists::fillPairHistograms(StEStructTrack* t1, StEStructTrack* t2, int ib, int after){
//};
//void StEStructQAHists::writePairHistograms(TFile* tf){
//};
/**********************************************************************
 *
 * $Log: StEStructQAHists.cxx,v $
 * Revision 1.11  2012/11/16 21:19:07  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.10  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.9  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.8  2010/06/23 22:29:47  prindle
 *   Hadd typo of 2004B instead of B2004 in EventCuts.cxx
 *   Added a couple of histograms in QAHists.
 *
 * Revision 1.7  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.6  2008/03/19 22:02:00  prindle
 * Updated some dataset definitions.
 *
 * Revision 1.5  2007/11/26 19:52:25  prindle
 * Add cucu62, cucu200 2007ib production datasets.
 * Included vertex cuts for case of ranked vertices. (Pass muDst pointer to EventCuts)
 * Add n^(1/4) histograms to QAHists
 *
 * Revision 1.4  2006/04/27 22:20:10  prindle
 * Some changes in trigger names for run periods.
 * Changed a couple of the Hijing QA histograms.
 *
 * Revision 1.3  2006/04/11 17:50:50  prindle
 *   Remove inChain from constructor arguments (no longer used in macro)
 *
 * Revision 1.2  2006/04/06 00:54:03  prindle
 *   Tried to rationalize the way centrality is defined.
 *   Now the reader gives a float to StEStructEvent and this float is
 * what is being used to define centrality. When we need a centrality
 * bin index we pass this number into the centrality singleton object.
 *
 *
 *
 * Revision 1.1  2006/04/04 22:05:06  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 *
 *********************************************************************/
