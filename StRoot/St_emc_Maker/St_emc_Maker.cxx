// $Id: St_emc_Maker.cxx,v 1.15 2017/04/26 20:22:16 perev Exp $
// $Log: St_emc_Maker.cxx,v $
// Revision 1.15  2017/04/26 20:22:16  perev
// Hide m_DataSet
//
// Revision 1.14  2007/04/28 17:56:05  perev
// Redundant StChain.h removed
//
// Revision 1.13  2003/09/02 17:59:29  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.12  2003/04/30 20:39:16  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.11  2000/01/29 00:04:58  akio
// temprary fix for endcap. need more work, but no more junk messages and crash
//
// Revision 1.10  1999/07/16 18:24:27  pavlinov
// Little correction for StEclMake
//
// Revision 1.9  1999/07/15 13:58:01  perev
// cleanup
//
// Revision 1.8  1999/07/02 03:01:56  pavlinov
// Little corrections for Linux
//
// Revision 1.7  1999/07/01 16:17:57  pavlinov
// class StEmcGeom was created and maker was remade for new maker scheme
//
// Revision 1.6  1999/03/03 04:12:15  fisyak
// replace kStErr to kStWarn
//
// Revision 1.5  1999/02/26 17:28:50  kathy
// fix histograms
//
// Revision 1.4  1999/02/16 18:15:44  fisyak
// Check in the latest updates to fix them
//
// Revision 1.3  1999/02/12 19:16:30  akio
// *** empty log message ***
//
// Revision 1.2  1998/12/15 22:39:49  akio
// Add emc_hit object and  adc_to_energy in here.
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_Maker class for <FONT COLOR="RED"> EMC</FONT> dataset         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <math.h>
#include "St_emc_Maker.h"
#include "St_DataSetIter.h"
ClassImp(St_emc_Maker)

const TString detname[] = {
  "bemc", "bprs", "bsmde", "bsmdp",
  "eemc", "eprs", "esmde", "esmdp"
};
//_____________________________________________________________________________
St_emc_Maker::St_emc_Maker(const char *name, const char *title):StMaker(name,title){
  drawinit=kFALSE;
}
//_____________________________________________________________________________
St_emc_Maker::~St_emc_Maker(){ /* Nobody */ }
//_____________________________________________________________________________
Int_t St_emc_Maker::Init(){
  Int_t i;
  //Making QA histgrams
  const char *title_h[] = {
    "Barrel EMC hits","Barrel PRS hits","Barrel SMD-Eta hits","Barrel SMD-Phi hits",
    "Endcap EMC hits","Endcap PRS hits","Endcap SMD-u hits",  "Endcap SMD-v hits"};
  const char *title_e[] = {
    "Barrel EMC energy","Barrel PRS energy","Barrel SMD-Eta energy","Barrel SMD-Phi energy",
    "Endcap EMC energy","Endcap PRS energy","Endcap SMD-u energy",  "Endcap SMD-v energy"};
  const Int_t   nx[] = {40,40,300,20,12,12,12,12};
  const Float_t xl[] = {-1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5};
  const Float_t xu[] = { 1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5};
  const Int_t   ny[] = {120, 120, 60, 900, 60, 60, 60, 60};
  m_nhit = new TH2F("EmcNHitsVsDet" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
  m_etot = new TH2F("EmcEtotVsDet" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
  for (i=0; i<MAXDET; i++){
    TString name_h = detname[i] + "Hits";
    TString name_e = detname[i] + "Energy";
    Float_t rpi = M_PI + 0.00001; 
    m_hits[i]   = new TH2F(name_h,title_h[i],nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
    m_energy[i] = new TH2F(name_e,title_e[i],nx[i],xl[i],xu[i],ny[i],-rpi, rpi);
  }

  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t St_emc_Maker::Make(){

  mEmcCalib   = GetInputDB("calib");
  if(!mEmcCalib){
    cout << "Warning in St_emc_Maker: Database not found" << endl;
    return kStWarn;;   
  }else{
    assert(mEmcCalib);
  }

  if (!GetData()->GetList()){   //if DataSet is empty, create object and fill it
    //Making Hits
    St_DataSet *emcRaw = GetDataSet("emc_raw/.data"); 
    if(!emcRaw) return kStWarn;
    St_DataSetIter itr(emcRaw);

    St_emc_hits *adc = 0;
    St_emc_hits dummy; TString tit = dummy.GetTitle();
    while ((adc = (St_emc_hits *)itr())) {
      if(adc->GetTitle() == tit){  // Get only emc_hits
	TString name = adc->GetName(); 
        name.ReplaceAll("emc_hits_","");
	for(int i=4; i<8; i++){if(detname[i]==name){goto SKIP;}} //only barrel for now
	StEmcHitCollection *hit = new StEmcHitCollection(name);
        hit->setEmcCalib(mEmcCalib);
        AddData(hit);
	if(hit->fill(adc) != kStOK) return kStWarn;
      }
    SKIP:;
    }
  }

  if(m_mode){     // make a copy in STAF table 'emc_hits' for STAF module
    St_DataSetIter itr(GetData());
    StEmcHitCollection *hit = 0;
    StEmcHitCollection dummy; TString tit = dummy.GetTitle();
    while ((hit = (StEmcHitCollection *)itr())) {
      if(hit->GetTitle() == tit){
	TString name_hits = "emc_hits_" + TString(hit->GetName());
        AddData(hit->copyToTable(name_hits));
      }
    }
  }

  MakeHistograms(); // Fill QA histgrams

  return kStOK;
}
//_____________________________________________________________________________
void St_emc_Maker::MakeHistograms(){
  //Filling QA Histograms
  if (GetData()) {
    Int_t det, i, n, id;
    Float_t eta, phi, ene;
    St_DataSetIter itr(GetData());
    StEmcHitCollection *hit = 0;
    StEmcHitCollection dummy;
    TString tit = dummy.GetTitle(); 
    while ((hit = (StEmcHitCollection *)itr())) {
       if(hit->GetTitle() == tit){	 
	n = hit->NHit();
	if(n>0){
	  det = hit->Detector();
	  m_nhit->Fill(log10((Float_t)hit->NHit()),(Float_t)det);
	  m_etot->Fill(log10(hit->EnergySum()),(Float_t)det);
	  for(i = 0; i < n; i++){
	    ene = hit->HitEnergy(i);
	    if(ene > 0.0){
              id = hit->HitId(i);
	      if(hit->getEtaPhi(id, eta, phi) == 0){
	        m_hits[det-1]->Fill(eta,phi);
	        m_energy[det-1]->Fill(eta,phi,ene);
              }
              else {
                printf(" <W> St_emc_Maker::MakeHistograms() => det %i bad id %i id energy %f \n"
                       ,det,id,ene);
              }
	    }    
	  }
	}
      }
    }
  }
}
//_____________________________________________________________________________
//_____________________________________________________________________________
