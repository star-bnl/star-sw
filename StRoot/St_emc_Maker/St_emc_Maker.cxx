// $Id: St_emc_Maker.cxx,v 1.2 1998/12/15 22:39:49 akio Exp $
// $Log: St_emc_Maker.cxx,v $
// Revision 1.2  1998/12/15 22:39:49  akio
// Add emc_hit object and  adc_to_energy in here.
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_emc_Maker class for <FONT COLOR="RED"> EMC</FONT> dataset   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include "/afs/rhic/star/packages/SL98j/pams/global/inc/math_constants.h"
#include "St_emc_Maker.h"
#include "StChain.h"
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
St_emc_Maker::~St_emc_Maker(){
}
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
  m_nhit = new TH2F("nhit_emc" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
  m_etot = new TH2F("etot_emc" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
  for (i=0; i<MAXDET; i++){
    TString name_h = detname[i] + "_hits";
    TString name_e = detname[i] + "_energy";
    m_hits[i]   = new TH2F(name_h,title_h[i],nx[i],xl[i],xu[i],ny[i],0.0,C_2PI);
    m_energy[i] = new TH2F(name_e,title_e[i],nx[i],xl[i],xu[i],ny[i],0.0,C_2PI);
  }
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t St_emc_Maker::Make(){
  if (!m_DataSet->GetList()){   //if DataSet is empty, create object and fill it
    //Making Hits
    St_DataSetIter itr(gStChain->DataSet("emc_raw"));
    //Int_t det;
    //for(det=0; det<MAXDET; det++){
    //  TString n_emc = "emc_hits_" + detname[det];
    //  St_emc_hits *emc = (St_emc_hits *)itr(n_emc);
    //  St_emc_hit *hit = new St_emc_hit(detname[det], det+1); 
    //  m_DataSet->Add(hit);
    //  if(hit->Fill(emc) != kStOK) return kStErr;
    //}
    St_emc_hits *adc = 0;
    St_emc_hits dummy; TString tit = dummy.GetTitle();
    while (adc = (St_emc_hits *)itr()) {
      if(adc->GetTitle() == tit){
	TString name = adc->GetName(); name.ReplaceAll("emc_hits_","");
	St_emc_hit *hit = new St_emc_hit(name); m_DataSet->Add(hit);
	if(hit->Fill(adc) != kStOK) return kStErr;
      }
    }
  }
  if(m_mode){     // make a copy in STAF table 'emc_hits' for STAF module
    St_DataSetIter itr(m_DataSet);
    // Int_t det;
    // for(det=0; det<MAXDET; det++){
    //   St_emc_hit *hit = (St_emc_hit *)itr(detname[det]);
    //   TString n_emc = "emc_hits_" + TString(hit->GetDetName());
    //   m_DataSet->Add(hit->CopyToTable(n_emc));
    // }
    St_emc_hit *hit = 0;
    St_emc_hit dummy; TString tit = dummy.GetTitle();
    while (hit = (St_emc_hit *)itr()) {
      if(hit->GetTitle() == tit){
	TString name_hits = "emc_hits_" + TString(hit->GetDetName());
        m_DataSet->Add(hit->CopyToTable(name_hits));
      }
    }
  }
  MakeHistograms(); // Fill QA histgrams
  return kStOK;
}
//_____________________________________________________________________________
void St_emc_Maker::MakeHistograms(){
  //Filling QA Histograms
  const Float_t offset = 0.000001;  // this is just to avoid binning probrem!
  if (m_DataSet) {
    Int_t det, i, n, ieta, iphi;
    Float_t eta, phi, ene;
    St_DataSetIter itr(m_DataSet);
    St_emc_hit *hit = 0;
    St_emc_hit dummy; TString tit = dummy.GetTitle();
    while (hit = (St_emc_hit *)itr()) {
       if(hit->GetTitle() == tit){	 
	n = hit->GetNraw();
	if(n>0){
	  det = hit->GetDetector();
	  m_nhit->Fill(log10((Float_t)hit->GetNhit()),(Float_t)det);
	  m_etot->Fill(log10(hit->GetEtot()),(Float_t)det);
	  for(i = 0; i < n; i++){
	    ene = hit->GetEnergy(i);
	    if(ene > 0.0){
	      hit->GetPos(i,&eta,&phi);
	      if(det>=EEMC){
		hit->GetGrid(i,&ieta,&iphi);
		eta=(Float_t)ieta;
	      }
	      m_hits[det-1]->Fill(eta,phi+offset);
	      m_energy[det-1]->Fill(eta,phi+offset,ene);	
	    }    
	  }
	}
      }
    }
  }
}
//_____________________________________________________________________________
void St_emc_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_emc_Maker.cxx,v 1.2 1998/12/15 22:39:49 akio Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
