//
// $Id: StPreEclMaker.cxx,v 1.7 2000/09/08 22:55:06 suaide Exp $
//
// $Log: StPreEclMaker.cxx,v $
// Revision 1.7  2000/09/08 22:55:06  suaide
// some modifications to compile on Solaris
//
// Revision 1.6  2000/09/08 21:48:00  suaide
//
//
// See README for details
//
// Revision 1.5  2000/08/24 22:11:35  suaide
//
//
// restored some files for background compatibility
//
// Revision 1.4  2000/08/24 19:45:37  suaide
//
//
// small modifications: some cout has been removed
//
// Revision 1.3  2000/08/24 11:26:48  suaide
//
//
//
// by A. A. P. Suaide - 2000/08/24 07:25:00
//
// Notes:
//
// 1. Full StEvent Compatible
// 2. Read hits from StEvent object
// 3. Write clusters in StEvent format and old format to keep background
//    compatibility
// 4. Do clustering in bemc, bprs, bsmde, bsmdp
// 5. Included method StPreEclMaker::SetClusterCollection
//
// Revision 1.2  2000/07/04 02:36:52  perev
// formal corrections, gStChain removed
//
// Revision 1.1  2000/05/15 21:24:01  subhasis
// initial version
//
//
// Authors: Alexandre A. P. Suaide (version 1.3)
//          Subhasis Chattopadhyay,
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

#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h"


ClassImp(StPreEclMaker)


int nModule[8] = {120, 120, 120, 120, 24, 24, 24, 24}; // temp.->database
const TString detname[] = {"bemc", "bprs", "bsmde", "bsmdp","eemc", "eprs", "esmde", "esmdp"};

TArrayI       mSizeMaxConf(8);      
TArrayF       mEnergySeedConf(8);
TArrayF       mEnergyAddConf(8);
TArrayF       mEnergyThresholdAllConf(8);
Bool_t        kCheckClustersOkConf[8];
StEmcCollection* emc;

//_____________________________________________________________________________
StPreEclMaker::StPreEclMaker(const char *name, const char *title):StMaker(name,title){
//  drawinit=kFALSE;
}
//_____________________________________________________________________________
StPreEclMaker::~StPreEclMaker()
{
}
//_____________________________________________________________________________
Int_t StPreEclMaker::Init()
{
  //Setting default cluster conditions ...
  SetClusterConditions("bemc",4,0.1,0.001,0.02,kFALSE);
  SetClusterConditions("bprs",4,0.1,0.001,0.2,kFALSE);
  SetClusterConditions("bsmde",5,0.08,0.001,0.001,kFALSE);
  SetClusterConditions("bsmdp",5,0.08,0.001,0.001,kFALSE);
  
  Int_t greta[4]={40,40,300,20}; // eta bins
  Int_t grphi[4]={40,40,60,900}; // phi bins
  
  //Making QA histgrams
  m_ncl  = new TH2F("Ncluster","Number of cluster(log) .vs. Detector #",40,0.0,4.0, 4,0.5,4.5);
  m_etot = new TH2F("Ecluster" ,"Total PreCluster Energy(log) .vs. Detector #", 40,-2.0,2.0, 4,0.5,4.5);
  Float_t rmsMax=0.026;
  Int_t   rmsN=52;
  m_sig_e= new TH2F("RMS(eta)" ,"Sigma(eta) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  m_sig_p= new TH2F("RMS(phi)" ,"Sigma(phi) .vs. Detector #",rmsN,0.0,rmsMax,4,0.5,4.5);
  for (Int_t i=0; i<4; i++)
  {
    TString name_h = detname[i] + "_cluster";
    TString name_e = detname[i] + "_cluster_energy";
    TString tit_h  = detname[i] + " cluster";
    TString tit_e  = detname[i] + " energy of cluster";
    m_cl[i]     = new TH2F(name_h,tit_h,greta[i],-1.0,1.0,grphi[i],-M_PI, M_PI);
    m_energy[i] = new TH2F(name_e,tit_e,greta[i],-1.0,1.0,grphi[i],-M_PI, M_PI);

    TString name_m  = detname[i] + "ClNum";
    TString tit_m   = "Number hits in cluster for " + detname[i];
    m_HitsInCl[i]   = new TH1F(name_m, tit_m, 21, -0.5, 20.5);

    TString name_en  = detname[i] + "ClEnergy";
    TString tit_en   = "Energy of cluster for " + detname[i];
    m_EnergyCl[i]    = new TH1F(name_en, tit_en, 2000, 0.0, 20.0);

    TString name_eta  = detname[i] + "Eta";
    TString tit_eta   = "Eta of clusters for " + detname[i];
    m_EtaInCl[i]   = new TH1F(name_eta, tit_eta, greta[i], -1., 1.);

    TString name_phi  = detname[i] + "Phi";
    TString tit_phi   = "Phi of clusters for " + detname[i];
    m_PhiInCl[i]   = new TH1F(name_phi, tit_phi, grphi[i], -M_PI, M_PI);
  }
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StPreEclMaker::Make()
{
  cout << "\n************************* Entering PreEclMaker Make() \n\n";

// First of all, try to get StEvent Pointer
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  
  if(!currevent)
  {
    cout << "***** Can not get StEvent pointer\n";
    kStEvOk=kFALSE;
  }
  else
  {
    cout << "***** StEvent pointer Ok\n";
    kStEvOk=kTRUE;
    emc=currevent->emcCollection();
  }
    
  if(!emc) // if not emcCollection, try to get from St_emc_Maker or creates one and fill it with hits from emc
  {
      cout <<"***** No emc object. Creating one ..\n";
      emc = new StEmcCollection();
      St_DataSetIter itr(GetDataSet("emc_hits"));
      StEmcHitCollection* hit=0;
      while((hit = (StEmcHitCollection*)itr()))
      {
        TString name = hit->GetName();
        for(Int_t i=0; i<8; i++)
        {  
          if(!strcmp(name.Data(),detname[i].Data()))
          {
            StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
            //Create StEmcDetector
            StEmcDetector* detector = new StEmcDetector(id, nModule[i]);
            //Create StEmcHit
            for(Int_t j=0; j<hit->NHit(); j++)
            {
	            Float_t energy=hit->HitEnergy(j);
              Int_t hid=hit->HitId(j);
              Int_t module,eta,sub;
              hit->getBin(hid,module,eta,sub);
              StEmcRawHit *rawHit=new StEmcRawHit(id,(UInt_t)module,(UInt_t)eta,(UInt_t)sub,0,energy);
  	          detector->addHit(rawHit);
            }
            emc->setDetector(detector);
          } 
        }
      }
    
    if(kStEvOk) 
    {
      currevent->setEmcCollection(emc);
      cout <<"***** New EmcCollection on StEvent\n";
    }
    
  }
// At this point, StEmcCollection should be Ok.
//

// Starting clustering....

  for(Int_t i=0; i<4; i++)  // Loop over the BEMC detectors
  {  
    cout <<"***** Doing clustering on detector "<<detname[i].Data()<<"\n";
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* mDet=emc->detector(id); // getting detector pointer
    StEmcPreClusterCollection* cc=new StEmcPreClusterCollection(detname[i].Data(),mDet);
    if(cc->IsOk())
    {
      m_DataSet->Add(cc);
      cc->setSizeMax(mSizeMaxConf[i]);
      cc->setEnergySeed(mEnergySeedConf[i]);
      cc->setEnergyAdd(mEnergyAddConf[i]);
      cc->setEnergyThresholdAll(mEnergyThresholdAllConf[i]);
      cc->setCheckClusters(kCheckClustersOkConf[i]);
//      cc->printConf();
  	  if(cc->findClusters()!= kStOK) return kStErr;
    }
  }

  MakeHistograms(); // Fill QA histgrams
  fillStEvent(); 
  
  AddData(new St_ObjectSet("PreEclEmcCollection",emc));
  cout <<"***** New EmcCollection on local .data\n";
    
  return kStOK;
}
//_____________________________________________________________________________
void StPreEclMaker::MakeHistograms()
{

  Int_t n,det;
  if (!m_DataSet) return;

  St_DataSetIter itr(m_DataSet);
  StEmcPreClusterCollection *cluster = 0;

  for(Int_t idet=0; idet <4; idet++)
  {
    cluster = (StEmcPreClusterCollection*)itr(detname[idet].Data());
    if(cluster > 0)
    {
      n = cluster->Nclusters();
      Float_t Etot=0.0;
      if(n>0)
      {
	      det = cluster->Detector();
        m_ncl->Fill(log10((Float_t)n),(Float_t)det);

        TIter next(cluster->Clusters());
        StEmcPreCluster *cl;
        for(Int_t i=0; i<n; i++)
        {
          cl=(StEmcPreCluster*)next();
          Float_t eta=cl->Eta();
          Float_t phi=cl->Phi();
          Float_t energy=cl->Energy();
          Float_t sigmaeta=cl->SigmaEta();
          Float_t sigmaphi=cl->SigmaPhi();
          Int_t   nhits=cl->Nhits();

          if(sigmaeta>1.0e-5) m_sig_e->Fill(sigmaeta, (Float_t)det);
          else m_sig_e->Fill(-1.,(Float_t)det);
          
          if(sigmaphi>1.0e-5) m_sig_p->Fill(sigmaphi, (Float_t)det);
          else m_sig_p->Fill(-1.,(Float_t)det);
          
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
  }
}

//------------------------------------------------------------------------
Int_t StPreEclMaker::fillStEvent()
{
  
  if (!m_DataSet) return kStOK;

  St_DataSetIter itr(m_DataSet);
  StEmcPreClusterCollection *cluster = 0;
 
  Char_t *cdet;                                   

  for(Int_t idet=0; idet <4; idet++)
  {
    
    cluster = (StEmcPreClusterCollection*)itr(detname[idet].Data());
    
    if(cluster> 0)
    {
      StDetectorId id = static_cast<StDetectorId>(idet+kBarrelEmcTowerId); 
      StEmcDetector* detector = emc->detector(id);           

      StEmcClusterCollection* cluscoll = new StEmcClusterCollection();
      cluscoll->setDetector(id);
      cluscoll->setClusterFinderId(1);
      cluscoll->setClusterFinderParamVersion(1); 

      detector->setCluster(cluscoll);
 
      Int_t n = cluster->Nclusters();
      if(n>0)
      {
        Int_t det = cluster->Detector(); 
        if(det){}
 
        TIter next(cluster->Clusters());
        StEmcPreCluster *cl;
        for(Int_t i=0; i<n; i++)
        {
          cl=(StEmcPreCluster*)next();
          Int_t module=cl->Module();
          StSPtrVecEmcRawHit& RawHit = detector->module((UInt_t)module)->hits();
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
            
          for(Int_t kk=0;kk<cl->Nhits();kk++)
          {
            clus->addHit(RawHit[cl->ID(kk)]);
          }   
          
          cluscoll->addCluster(clus);
        } 
      } 
    } 
  } 
  cout <<"**********************************************************************\n"; 
  return kStOK;
}                                                                               
//_____________________________________________________________________________
void StPreEclMaker::SetClusterConditions(char *cdet,Int_t mSizeMax,
                                            Float_t mEnergySeed,
                                            Float_t mEnergyAdd,
                                            Float_t mEnergyThresholdAll)
{
  for(Int_t i=0;i<8;i++)
  {
    if(!strcmp(cdet,detname[i].Data()))
    {
      mSizeMaxConf[i]=mSizeMax;
      mEnergySeedConf[i]=mEnergySeed;
      mEnergyAddConf[i]=mEnergyAdd;
      mEnergyThresholdAllConf[i]=mEnergyThresholdAll;
    }
  }
}
//_____________________________________________________________________________
void StPreEclMaker::SetClusterConditions(char *cdet,Int_t mSizeMax,
                                            Float_t mEnergySeed,
                                            Float_t mEnergyAdd,
                                            Float_t mEnergyThresholdAll,
                                            Bool_t  kCheckClustersOk)
{
  SetClusterConditions(cdet,mSizeMax,mEnergySeed,mEnergyAdd,mEnergyThresholdAll);
  for(Int_t i=0;i<8;i++)
  {
    if(!strcmp(cdet,detname[i].Data()))
    {
      kCheckClustersOkConf[i]=kCheckClustersOk;
    }
  }
}
//_____________________________________________________________________________
void StPreEclMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StPreEclMaker.cxx,v 1.7 2000/09/08 22:55:06 suaide Exp $   \n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
