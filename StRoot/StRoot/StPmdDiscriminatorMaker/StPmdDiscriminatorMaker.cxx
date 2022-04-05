/***************************************************************
 *
 * $Id: StPmdDiscriminatorMaker.cxx,v 1.10 2007/04/26 04:13:19 perev Exp $
 * Author: Subhasis Chattopadhyay
 ***************************************************************
 *
 * Description: StPmdDiscriminationMaker is class for photon/hadron
 * discrimination 
 *
 ****************************************************************
 * $Log: StPmdDiscriminatorMaker.cxx,v $
 * Revision 1.10  2007/04/26 04:13:19  perev
 * Remove StBFChain dependency
 *
 * Revision 1.9  2005/10/21 00:54:06  subhasis
 * fillStEvent crash due to mismatch in length of pmdclust and phmdclust fixed
 *
 * Revision 1.8  2005/02/23 05:17:18  subhasis
 * DiscbyNN option fixed
 *
 * Revision 1.7  2004/10/30 00:08:00  subhasis
 * TranFlag added and set to 0 in ctor
 *
 * Revision 1.6  2004/07/16 14:29:32  subhasis
 * more checks on edep Discriminate
 *
 * Revision 1.5  2004/07/13 12:59:11  subhasis
 *  Ncluster() is put with check in PrepareInput()
 *
 * Revision 1.4  2003/09/02 17:58:48  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/08/04 18:53:44  perev
 * warnOff
 *
 * Revision 1.2  2003/05/29 13:12:51  subhasis
 * several changes to include NN
 *
 ****************************************************************/
#include<Stiostream.h>
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

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
  mTrainFlag=0;
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
	  if(discbyNN){
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
      cout<<"calling edep_disc "<<endl;

      disc->Discriminate();   //! Discrimination through Energy cut
    //  disc->Print();  //! Print photon like hits
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
  Int_t nclust=0;
  if(clusters){nclust = clusters->Nclusters();}

  if(nclust>0){
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
}


Int_t StPmdDiscriminatorMaker::PrepareInputforNN(StPmdDetector* pmd_det,StPmdDetector* cpv_det,StPhmdClusterCollection* cluscoll,StPhmdClusterCollection* cpvcluscoll)
  
{
  cout<<"prepareInput***"<<endl;
  Double_t etapmd,phipmd,edeppmd;
  Double_t etacpv,phicpv,edepcpv;

  Bool_t mcpvdone;
  
  for(Int_t ibineta=0;ibineta<400;ibineta++){
    for(Int_t ibinphi=0;ibinphi<1400;ibinphi++){
      EtaPhiArr_CPV[ibineta][ibinphi]=-999;
    }
  }
  
  StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  StPmdClusterCollection* clusters1 = (StPmdClusterCollection*)cpv_det->cluster();
  //  Int_t nclust = clusters->Nclusters(); 

  if(cluscoll){
    Int_t Ncluster0=cluscoll->numberOfclusters();
    if(Ncluster0>0)
      {
	const StSPtrVecPhmdCluster& pmdclusters= cluscoll->clusters();
	const StSPtrVecPhmdCluster& cpvclusters= cpvcluscoll->clusters();
	
	cout<<"PMD cluster size "<<pmdclusters.size()<<endl;
	cout<<"CPV cluster size "<<cpvclusters.size()<<endl;
	
if(clusters1){
	  Int_t nclust1=0;
	  nclust1 = clusters1->Nclusters(); 
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
	    if(fabs(phicpv)<3.14){phibin=Int_t((phicpv+3.14)/mDeltaPhi);}
	    EtaPhiArr_CPV[etabin][phibin]=icpv;
	  }
  }	


	Int_t cpvmatched=0;
	Int_t hadmatched=0;
  StPmdCluster *spmcl1;
  if(clusters){
	  TIter next(clusters->Clusters());
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
	    if(fabs(phipmd)<3.14)phibin=Int_t((phipmd+3.14)/mDeltaPhi);
	    
	    for(Int_t ibineta=0;ibineta<400;ibineta++){
	      for(Int_t ibinphi=0;ibinphi<1400;ibinphi++){
		if(EtaPhiArr_CPV[ibineta][ibinphi]!=-999 && EtaPhiArr_CPV[ibineta][ibinphi]!=0){
		  Float_t etadiff=abs(ibineta-etabin);
		  Float_t phidiff=abs(ibinphi-phibin);
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
	  }
	if(cpvmatched>0)mcpvmatched->Fill(cpvmatched);
	
      }
  }
  return 0;
}

Int_t StPmdDiscriminatorMaker::Finish()
{
  if(mApplyFlagNN!=1 && mTrainFlag==1){
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
	  int nclust_phmd=pmdclusters.size();

	  int loopcls=0;
	  if(nclust==nclust_phmd)loopcls=nclust;
	  if(nclust<nclust_phmd)loopcls=nclust;
	  if(nclust>nclust_phmd)loopcls=nclust_phmd;

	  for(Int_t i=0; i<loopcls ; i++)
	    {
	      spmcl1 = (StPmdCluster*)next();
	      int edeppid=spmcl1->CluEdepPID();
	      StPhmdCluster *cl1=(StPhmdCluster*)pmdclusters[i];
		if(!cl1)break;
	      if(cl1)cl1->setEnergyPid(edeppid);
	    }
	}
      }
    }
  }
}

