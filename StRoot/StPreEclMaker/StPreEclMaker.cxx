//
// $Id: StPreEclMaker.cxx,v 1.1 2000/05/15 21:24:01 subhasis Exp $
//
// $Log: StPreEclMaker.cxx,v $
// Revision 1.1  2000/05/15 21:24:01  subhasis
// initial version
//
//
// Authors: Subhasis Chattopadhyay,
//          Aleksei Pavlinov , July 1999.
//          initial version from Akio Ogawa    
//    

//////////////////////////////////////////////////////////////////////////
//                                                             
// StPreEclMaker class  
//                                                             
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StPreEclMaker.h"

// added for StEvent
 
#include "St_ObjectSet.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StEmcClusterCollection.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEnumerations.h"


ClassImp(StPreEclMaker)

const TString detname[] = {"Bemc", "Bprs", "Bsmde", "Bsmdp"};

//_____________________________________________________________________________
StPreEclMaker::StPreEclMaker(const char *name, const char *title):StMaker(name,title){
//  drawinit=kFALSE;
}
//_____________________________________________________________________________
StPreEclMaker::~StPreEclMaker(){
}
//_____________________________________________________________________________
Int_t StPreEclMaker::Init(){
  Int_t i;
  //Making QA histgrams
  m_ncl  = new TH2F("Ncluster","Number of cluster(log) .vs. Detector #",40,0.0,4.0, 4,0.5,4.5);
  m_etot = new TH2F("Ecluster" ,"Total PreCluster Energy(log) .vs. Detector #", 40,-2.0,2.0, 4,0.5,4.5);
  Float_t rmsMax=0.026;
  Int_t   rmsN=52;
  m_sig_e= new TH2F("RMS(eta)" ,"Sigma(eta) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  m_sig_p= new TH2F("RMS(phi)" ,"Sigma(phi) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  for (i=0; i<4; i++){
    TString name_h = detname[i] + "_cluster";
    TString name_e = detname[i] + "_cluster_energy";
    TString tit_h  = detname[i] + " cluster";
    TString tit_e  = detname[i] + " energy of cluster";
    m_cl[i]     = new TH2F(name_h,tit_h,100,-1.0,1.0,100,-M_PI, M_PI);
    m_energy[i] = new TH2F(name_e,tit_e,100,-1.0,1.0,100,-M_PI, M_PI);

    TString name_m  = detname[i] + "ClNum";
    TString tit_m   = "Number hits in cluster for " + detname[i];
    m_HitsInCl[i]   = new TH1F(name_m, tit_m, 21, -0.5, 20.5);

    TString name_en  = detname[i] + "ClEnergy";
    TString tit_en   = "Energy of cluster for " + detname[i];
    m_EnergyCl[i]    = new TH1F(name_en, tit_en, 1000, 0.0, 10.0);

    TString name_eta  = detname[i] + "Eta";
    TString tit_eta   = "Eta of clusters for " + detname[i];
    m_EtaInCl[i]   = new TH1F(name_eta, tit_eta, 100, -1., 1.);

    TString name_phi  = detname[i] + "Phi";
    TString tit_phi   = "Phi of clusters for " + detname[i];
    m_PhiInCl[i]   = new TH1F(name_phi, tit_phi, 100, -4., 4.);
  }
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StPreEclMaker::Make(){
  if (!m_DataSet->GetList()){   //if DataSet is empty, create object and fill it
    StEmcHitCollection *hit = 0;
    StEmcHitCollection dummy; TString tit = dummy.GetTitle();
    St_DataSetIter itr(gStChain->DataSet("emc_hits"));
    while ( (hit = (StEmcHitCollection *)itr()) ) {
      if(hit->GetTitle() == tit){
	TString name = hit->GetName();
        Int_t nhitsw = hit->NHit();

        if(nhitsw > 0){ 
//          cout <<" Name "<< name.Data() <<"  Hits " << nhitsw << endl;
          if(!strcmp(name.Data(),"bemc")){
            StBemcPreClusterCollection *cc = new StBemcPreClusterCollection(name);
	    m_DataSet->Add(cc);
	    if(cc->findClusters(hit)!= kStOK) return kStErr;
          }
          else if(!strcmp(name.Data(),"bsmde")){
	    StBsmdePreClusterCollection *cc = new StBsmdePreClusterCollection(name);
	    m_DataSet->Add(cc);
	    if(cc->findClusters(hit)!= kStOK) return kStErr;
          }
          else if(!strcmp(name.Data(),"bsmdp")){
	    StBsmdpPreClusterCollection *cc = new StBsmdpPreClusterCollection(name);
	    m_DataSet->Add(cc);
	    if(cc->findClusters(hit)!= kStOK) return kStErr;
          }

        }
        else{
          cout <<" Name "<< hit->GetName()<<"  Hits " << nhitsw << endl;
        }
      }
    }
  }
  MakeHistograms(); // Fill QA histgrams
  Int_t fill = fillStEvent();
  return kStOK;
}
//_____________________________________________________________________________
void StPreEclMaker::MakeHistograms()
{
  Int_t n,det;
  if (!m_DataSet) return;

  St_DataSetIter itr(m_DataSet);
  StEmcPreClusterCollection *cluster = 0;
  StEmcPreClusterCollection dummy;
  TString tit = dummy.GetTitle(); 

  Char_t *cdet;
  for(Int_t idet=0; idet <4; idet++){
    switch (idet) {
    case 0: 
      cdet="bemc"; 
      cluster = (StBemcPreClusterCollection*)itr(cdet);
      break;
    case 1: 
      cdet="bprs"; 
      cluster = (StBemcPreClusterCollection*)itr(cdet);
      break;
    case 2: 
      cdet="bsmde"; 
      cluster = (StBsmdePreClusterCollection*)itr(cdet);
      break;
    case 3: 
      cdet="bsmdp";
      cluster = (StBsmdpPreClusterCollection*)itr(cdet);
      break;
    }
    if(cluster == 0) goto END;

    if(cluster->GetTitle() == tit){
      n = cluster->Nclusters();
      Float_t Etot=0.0;
      if(n>0){
	det = cluster->Detector();
        m_ncl->Fill(log10((Float_t)n),(Float_t)det);

        TIter next(cluster->Clusters());
        StEmcPreCluster *cl;
        for(Int_t i=0; i<n; i++){
          cl=(StEmcPreCluster*)next();
          Float_t eta=cl->Eta();
          Float_t phi=cl->Phi();
          Float_t energy=cl->Energy();
          Float_t sigmaeta=cl->SigmaEta();
          Float_t sigmaphi=cl->SigmaPhi();
          Int_t   nhits=cl->Nhits();

          if(sigmaeta>1.0e-5) m_sig_e->Fill(sigmaeta, (Float_t)det);
          else             m_sig_e->Fill(-1.,      (Float_t)det);
          if(sigmaphi>1.0e-5) m_sig_p->Fill(sigmaphi, (Float_t)det);
          else             m_sig_p->Fill(-1.,      (Float_t)det);
          m_cl[det-1]->Fill(eta, phi);
          m_energy[det-1]->Fill(eta, phi, energy);
          m_HitsInCl[det-1]->Fill((Float_t)nhits);
          m_EnergyCl[det-1]->Fill(energy);
          m_EtaInCl[det-1]->Fill(eta);
          m_PhiInCl[det-1]->Fill(phi);

          Etot+=energy;         
        }
        m_etot->Fill(log10(Etot),(Float_t)det);
      }
    }
END:
    continue;
  }
}

//------------------------------------------------------------------------
Int_t StPreEclMaker::fillStEvent(){
 
  int nModule[8] = {120, 120, 120, 120, 24, 24, 24, 24}; // temp.->database
  if (!m_DataSet) return kStOK;
 
  //Create StEmcHitCollection
  StEmcCollection* emc = new StEmcCollection();
 
  //Add it to dataset as ObjectSet
  AddData(new St_ObjectSet("EmcCollection" , emc));
 
  St_DataSetIter itr(m_DataSet);
  StEmcPreClusterCollection *cluster = 0;
  StEmcPreClusterCollection dummy;
  TString tit = dummy.GetTitle();
 
  Char_t *cdet;                                   

  for(Int_t idet=0; idet <4; idet++){
    switch (idet) {
    case 0:
      cdet="bemc";
      cluster = (StBemcPreClusterCollection*)itr(cdet);
      break;
    case 1:
      cdet="bprs";
      cluster = (StBemcPreClusterCollection*)itr(cdet);
      break;
    case 2:
      cdet="bsmde";
      cluster = (StBsmdePreClusterCollection*)itr(cdet);
      break;
    case 3:
      cdet="bsmdp";
      cluster = (StBsmdpPreClusterCollection*)itr(cdet);
      break;
    }
    if(cluster> 0){
// Set detector Id
    StDetectorId id = static_cast<StDetectorId>(idet+kBarrelEmcTowerId);
    //cout<<" after stdetectorId Id "<<id<<endl;
// Create StEmcDetector
 
      StEmcDetector* detector = new StEmcDetector(id, nModule[idet]);
 
//  Set Detector to EmcCollection
 
      emc->setDetector(detector);
 
// Create Cluster Collection and set Detector, clusterfinderId,
// ClusterFinderParamVersion  
 
      StEmcClusterCollection* cluscoll = new StEmcClusterCollection();
      cluscoll->setDetector(id);
      cluscoll->setClusterFinderId(1);
      cluscoll->setClusterFinderParamVersion(1);
 
//  set clustercoll to detector
 
      detector->setCluster(cluscoll);
 
    if(cluster->GetTitle() == tit){
      Int_t n = cluster->Nclusters();
      if(n>0){
        Int_t det = cluster->Detector();
 
        TIter next(cluster->Clusters());
        StEmcPreCluster *cl;
        for(Int_t i=0; i<n; i++){
            cl=(StEmcPreCluster*)next();
 
          Float_t eta=cl->Eta();
          Float_t phi=cl->Phi();
          Float_t energy=cl->Energy();
          Float_t sigmaeta=cl->SigmaEta();
          Float_t sigmaphi=cl->SigmaPhi();
 
                StEmcCluster* clus = new StEmcCluster();
                 clus->setEta(eta);
                 clus->setPhi(phi);
                 clus->setEnergy(energy);
                 clus->setSigmaEta(sigmaeta);
                 clus->setSigmaPhi(sigmaphi);
// Add cluster to clustercoll
 
           cluscoll->addCluster(clus);
 
         } // for loop for n
       } // if n>0 check
     } //tit check
   } // if cluster>0
    } // det loop
 
  return kStOK;
}                                                                               

//_____________________________________________________________________________
void StPreEclMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StPreEclMaker.cxx,v 1.1 2000/05/15 21:24:01 subhasis Exp $   \n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
