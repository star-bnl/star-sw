/***************************************************************
 *
 * $Id: StPmdDiscriminatorMaker.cxx,v 1.2 2003/05/29 13:12:51 subhasis Exp $
 * Author: Subhasis Chattopadhyay
 ***************************************************************
 *
 * Description: StPmdDiscriminationMaker is class for photon/hadron
 * discrimination 
 *
 ****************************************************************
 * $Log: StPmdDiscriminatorMaker.cxx,v $
 * Revision 1.2  2003/05/29 13:12:51  subhasis
 * several changes to include NN
 *
 ****************************************************************/
#include<iostream.h>
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

#include "StBFChain.h"
#include "StPmdDiscriminator.h"
#include "StPmdDiscriminatorMaker.h"
#include "StPmdDiscriminatorNN.h"
#include "StPmdUtil/StPmdCollection.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdUtil/StPmdClusterCollection.h"
#include "StPmdUtil/StPmdCluster.h"
#include "StNNCluster.h"
#include "StEventTypes.h"

StPmdCl NNContainer;
Int_t EventProcessed=0;
Int_t EtaPhiArr_CPV[400][1400];

ClassImp(StPmdDiscriminatorMaker)
  TDataSet *clusterIn;
StPmdDiscriminatorMaker *discm; 
StPmdCollection *cluster_hit;
//! StPmdCluster *pid;

bool discbyNN=false;
StPmdDiscriminatorMaker::StPmdDiscriminatorMaker(const char *name):StMaker(name){
  mApplyFlagNN=0;
}

StPmdDiscriminatorMaker::~StPmdDiscriminatorMaker()
{
  // !destructor
}

Int_t StPmdDiscriminatorMaker::Init()
{
  bookHistograms();
  return StMaker::Init();
}

void StPmdDiscriminatorMaker::bookHistograms()
{
  mEdepPmd = new TH1F("EdepPmd","Energy deposited",100,0.,0.02);
  mEtaPmd  = new TH1F("EtaPmd","Eta Distribution",100,-6., 6.);
  mPhiPmd  = new TH1F("PhiPmd","Phi Distribution",100,-3.14,3.14);
  mMCPID  = new TH1F("McPID","Mc PID",11,-0.5,10.5);
  mcpvmatched  = new TH1F("cpvmatched","CPV Matched",100,0,100.);
  mEtadiff  = new TH1F("DeltaEta","Eta Difference",200,0.,200.);
  mPhidiff  = new TH1F("DeltaPhi","Phi Difference",4000,0.,3000.);
  mEtaPhi = new TH2F("DeltaEtaDetaPhi","(Delta) Eta Phi Distribution",100,0,200.,4000,0.,3000.);
  mClusterPID  = new TH1F("ClusterPID","ClusterPID based on matching",10,0.5,10.5);
  mClusterEdepPID  = new TH1F("ClusterEdepPID","ClusterPID based on Edep",10,0.5,10.5);
  mEdepVsPID  = new TH2F("EdepVsPID","PID based on Edep",100,0.,200.,100,0.,10.);
  mNNoutput  = new TH1F("NNTrainOut","Training output from NN",100,0.,2.);
}

Int_t StPmdDiscriminatorMaker::Make()
{
  EventProcessed++;
  
  //! First of all, try to get StEvent Pointer
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  if(!currevent){
    cout << "PMDDISC ***** Can not get StEvent pointer . sorry \n";                     return kStErr;
  }
  
  StPhmdCollection* phmdcl = (StPhmdCollection*)currevent->phmdCollection();        
  clusterIn = GetDataSet("PmdSimulator");
  cluster_hit = (StPmdCollection*)clusterIn->Find("PmdCollection");
  StPmdDetector* pmd_det;
  StPmdDetector* cpv_det;
  if(cluster_hit){
    pmd_det = cluster_hit->detector(Int_t(0));
    cpv_det = cluster_hit->detector(Int_t(1));
  }
  
  if(phmdcl){
    StPhmdClusterCollection* cluscoll;
    StPhmdClusterCollection* cpvcluscoll;
    for(Int_t d=0;d<2;d++){
      StDetectorId pdet=static_cast<StDetectorId>(kPhmdCpvId+d);
      StPhmdDetector* detector=(StPhmdDetector*)phmdcl->detector(StDetectorId(pdet));
      if(detector){
	if(d==1)cluscoll = detector->cluster();
	if(d==0)cpvcluscoll = detector->cluster();
	if(d==1){
	  if(!discbyNN){
	    Int_t ret=PrepareInputforNN(pmd_det,cpv_det,cluscoll,cpvcluscoll);
	    if(ret==1)goto DISC;
	  }
	}
	if(d==1&&cluscoll)cout<<"ncls "<<cluscoll->numberOfclusters()<<endl;
      }
    }
  }
  cout<<"DISCMAKER*****"<<mApplyFlagNN<<endl;
  if(mApplyFlagNN==1){
    
    StPmdDiscriminatorNN *discNN = new StPmdDiscriminatorNN(NNContainer);
    discNN->setApplyFlag(1);
    discNN->setDisMaker(this);

    if(discNN)
      {
	discNN->Discriminate();
      }
    NNContainer.clear();
    
  }
  
 DISC:
  
  if(cluster_hit){
    StPmdDetector * pmd_det = cluster_hit->detector(Int_t(0));
    StPmdDetector * cpv_det = cluster_hit->detector(Int_t(1));
    StPmdDiscriminator *disc = new StPmdDiscriminator(mEdepThreshold, pmd_det, cpv_det);
    
    if(disc){
      disc->SetEdepcut(0.0000063);  //! 3 MIP cut
      disc->Discriminate();   //! Discrimination through Energy cut
      disc->Print();  //! Print photon like hits
    }
    fillHistograms(pmd_det,cpv_det);
  }	
  
  ///////////////////////////////
  
  fillStEvent(cluster_hit,phmdcl);
  
  ////////////////////////////////////////////////
  
  return kStOK;
}


void StPmdDiscriminatorMaker::Browse(TBrowser *b)
{
  TDataSet::Browse(b);
}

void StPmdDiscriminatorMaker::fillHistograms(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  
  Int_t nclust = clusters->Nclusters();
  TIter next(clusters->Clusters());
  StPmdCluster *spmcl1;
  for(Int_t i=0; i<nclust ; i++)
    {
      spmcl1 = (StPmdCluster*)next();
      //      Float_t eta=spmcl1->CluEta();
      //      Float_t phi=spmcl1->CluPhi();
      Float_t edep=spmcl1->CluEdep();
      Int_t PID=spmcl1->CluPID();
      Int_t EdepPID=spmcl1->CluEdepPID();
      
	 mEdepVsPID->Fill(edep*1000000,Float_t(EdepPID));
	 mClusterPID->Fill(Float_t(PID));
	 mClusterEdepPID->Fill(Float_t(EdepPID));
    }
}


Int_t StPmdDiscriminatorMaker::PrepareInputforNN(StPmdDetector* pmd_det,StPmdDetector* cpv_det,StPhmdClusterCollection* cluscoll,StPhmdClusterCollection* cpvcluscoll)
  
{
  cout<<"prepareInput***"<<endl;
  Double_t etapmd,phipmd,edeppmd;
  Double_t etacpv,phicpv,edepcpv;

  Float_t cutcluster=0;
  Bool_t mcpvdone;
  
  for(Int_t ibineta=0;ibineta<400;ibineta++){
    for(Int_t ibinphi=0;ibinphi<1400;ibinphi++){
      EtaPhiArr_CPV[ibineta][ibinphi]=-999;
    }
  }
  
  StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  StPmdClusterCollection* clusters1 = (StPmdClusterCollection*)cpv_det->cluster();
  //  Int_t nclust = clusters->Nclusters(); 
  Int_t nclust1 = clusters1->Nclusters(); 
  TIter next(clusters->Clusters());
  StPmdCluster *spmcl1;
  
  if(cluscoll){
    Int_t Ncluster0=cluscoll->numberOfclusters();
    if(Ncluster0>0)
      {
	const StSPtrVecPhmdCluster& pmdclusters= cluscoll->clusters();
	const StSPtrVecPhmdCluster& cpvclusters= cpvcluscoll->clusters();
	
	cout<<"PMD cluster size "<<pmdclusters.size()<<endl;
	cout<<"CPV cluster size "<<cpvclusters.size()<<endl;
	
	TIter nextCPV(clusters1->Clusters());
	StPmdCluster *spmcl2;
	for(Int_t icpv=0; icpv<nclust1 ; icpv++)
	  {
	    Int_t etabin,phibin;
	    spmcl2 = (StPmdCluster*)nextCPV();
	    etacpv=spmcl2->CluEta();
	    phicpv=spmcl2->CluPhi();
	    edepcpv=spmcl2->CluEdep();
	    
	    if(fabs(etacpv)>2. && fabs(etacpv)<4.){etabin=Int_t((fabs(etacpv)-2)/mDeltaEta);}
	    //if(fabs(etacpv)>2. && fabs(etacpv)<4.){etabin=Int_t((fabs(etacpv)-2)/0.2);}
	    if(fabs(phicpv)<3.14){phibin=Int_t((phicpv+3.14)/mDeltaPhi);}
	    // if(fabs(phicpv)<3.14){phibin=Int_t((phicpv+3.14)/0.2);}
	    EtaPhiArr_CPV[etabin][phibin]=icpv;
	  }
	
	Int_t cpvmatched=0;
	Int_t hadmatched=0;
	for(UInt_t i=0;i<pmdclusters.size();i++)
	  {
	    Int_t etabin,phibin;
	    mcpvdone=true;
	    spmcl1 = (StPmdCluster*)next();
	    StPhmdCluster *cl1=(StPhmdCluster*)pmdclusters[i];
	    cl1->setMcPid(spmcl1->McCluPID());
	    
	    StNNCluster* nncls = new StNNCluster();
	    nncls->setPmdCluster(cl1);
	    
            etapmd=spmcl1->CluEta();
            phipmd=spmcl1->CluPhi();
            edeppmd=spmcl1->CluEdep();
	    mMCPID->Fill(Float_t(spmcl1->McCluPID()));
	    
	    if(fabs(etapmd)>2. && fabs(etapmd)<4.)etabin=Int_t((fabs(etapmd)-2)/mDeltaEta);
	    //	 if(fabs(etapmd)>2. && fabs(etapmd)<4.)etabin=Int_t((fabs(etapmd)-2)/0.2);
	    if(fabs(phipmd)<3.14)phibin=Int_t((phipmd+3.14)/mDeltaPhi);
	    //	 if(fabs(phipmd)<3.14)phibin=Int_t((phipmd+3.14)/0.2);
	    
	    
	    for(Int_t ibineta=0;ibineta<400;ibineta++){
	      for(Int_t ibinphi=0;ibinphi<1400;ibinphi++){
		if(EtaPhiArr_CPV[ibineta][ibinphi]!=-999 && EtaPhiArr_CPV[ibineta][ibinphi]!=0){
		  Float_t etadiff=fabs(ibineta-etabin);
		  Float_t phidiff=fabs(ibinphi-phibin);
	          mEtaPhi->Fill(etadiff,phidiff);
	          mEtadiff->Fill(etadiff);
	          mPhidiff->Fill(phidiff);
		}
	      }
	    }
	    
	    if(EtaPhiArr_CPV[etabin][phibin]!=-999){
	      Int_t index=EtaPhiArr_CPV[etabin][phibin];
	      nncls->setCpvCluster((StPhmdCluster*)cpvclusters[index]);
	      if(spmcl1->McCluPID()==8){
		hadmatched++;
		spmcl1->setCluPID(8); //PID 8 set for photon
	      }
	      if(spmcl1->McCluPID()==1){
		hadmatched++;
		spmcl1->setCluPID(1); //PID 8 set for photon
	      }
	      cpvmatched++;
	    }
	    
	    
	    NNContainer.push_back(nncls); 
	  }
	if(cpvmatched>0)mcpvmatched->Fill(cpvmatched);
	
      }
  }
  return 0;
}

Int_t StPmdDiscriminatorMaker::Finish()
{
  if(mApplyFlagNN!=1){
    StPmdDiscriminatorNN *discNN = new StPmdDiscriminatorNN(NNContainer);
    discNN->setApplyFlag(0);
    discNN->setDisMaker(this);
    if(discNN)
      {
	discNN->Discriminate();
      } 	
  }
  return StMaker::Finish();
}



void StPmdDiscriminatorMaker::fillStEvent(StPmdCollection* cluster_hit,StPhmdCollection* phmdcl)
{
  
  if(cluster_hit){
    StPmdDetector * pmd_det = cluster_hit->detector(Int_t(0));
    StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
    if(phmdcl){

      StDetectorId pdet=static_cast<StDetectorId>(kPhmdId);
      StPhmdDetector* detector=(StPhmdDetector*)phmdcl->detector(StDetectorId(pdet));
      if(detector){
	StPhmdClusterCollection* cluscoll = detector->cluster();
        if(cluscoll){
	  const StSPtrVecPhmdCluster& pmdclusters= cluscoll->clusters();
	  Int_t nclust = clusters->Nclusters();
	  TIter next(clusters->Clusters());
	  StPmdCluster *spmcl1;
	  for(Int_t i=0; i<nclust ; i++)
	    {
	      spmcl1 = (StPmdCluster*)next();
	      int edeppid=spmcl1->CluEdepPID();
	      StPhmdCluster *cl1=(StPhmdCluster*)pmdclusters[i];
	      cl1->setEnergyPid(edeppid);
	    }
	}
      }
    }
  }
}

