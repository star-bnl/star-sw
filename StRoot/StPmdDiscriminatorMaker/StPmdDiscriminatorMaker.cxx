/***************************************************************
 *
 * $Id: StPmdDiscriminatorMaker.cxx,v 1.1 2002/08/27 12:10:06 subhasis Exp $
 * Author: Subhasis Chattopadhyay
 *         Gopika Sood
 ***************************************************************
 *
 * Description: StPmdDiscriminationMaker is class for photon/hadron
 * discrimination 
 *
 ****************************************************************
 * $Log: StPmdDiscriminatorMaker.cxx,v $
 * Revision 1.1  2002/08/27 12:10:06  subhasis
 * First version
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
#include "StPmdUtil/StPmdCollection.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdUtil/StPmdClusterCollection.h"
#include "StPmdUtil/StPmdCluster.h"

ClassImp(StPmdDiscriminatorMaker)
  TDataSet *clusterIn;
StPmdDiscriminatorMaker *discm; 
    StPmdCollection *cluster_hit;

StPmdDiscriminatorMaker::StPmdDiscriminatorMaker(const char *name):StMaker(name){
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
  mAboveEdep = new TH1F("AboveEdep","No of cls above edepcut",100,-0.5,99.5);
  mEdepPmd = new TH1F("EdepPmd","Energy deposited",100,0.,0.02);
  mEtaPhi = new TH2F("DeltaEtaDetaPhi","(Delta) Eta Phi Distribution",100,-6.,6.,100,-3.14,3.14);
  mEtaPhim = new TH2F("EtaPhiMatched","Eta Phi Distribution Matched",100,-6.,6.,100,-3.14,3.14);
  mPmdClusterm = new TH2F("mPmdClusterm","PmdCluster Distribution",100,0.,50.,100,0.,100); 
  mCpvClusterm = new TH2F("mCpvClusterm","CpvCluster Distribution",100,0.,50.,100,0.,100); 
  mClusterPID  = new TH1F("ClusterPID","ClusterPID based on matching",10,0.5,10.5);
  mClusterEdepPID  = new TH1F("ClusterEdepPID","ClusterPID based on Edep",10,0.5,10.5);
}

Int_t StPmdDiscriminatorMaker::Make()
{
	   SetDeltaEta(0.01); // Delta-Eta value set for Matching
	   SetDeltaPhi(0.01); // Delta-Phi value set for Matching
	    
clusterIn = GetDataSet("PmdSimulator");
cluster_hit = (StPmdCollection*)clusterIn->Find("PmdCollection");

if(cluster_hit){
  StPmdDetector * pmd_det = cluster_hit->detector(Int_t(0));
  StPmdDetector * cpv_det = cluster_hit->detector(Int_t(1));
     StPmdDiscriminator *disc = new StPmdDiscriminator(mEdepThreshold, pmd_det, cpv_det);
     if(disc){
       disc->SetEdepcut(0.0000063);  // 3 MIP
       disc->Discriminate();   // Discrimination through Energy cut
       disc->Print();  // Print photon like hits
     }
     disc->getClusterPID(); // Gives the cluster PID through Energy cut
     Matching(pmd_det,cpv_det); // Discrimnation through Matching PMD, CPV hits
     fillHistograms(pmd_det,cpv_det);
   }	
return kStOK;
}



void StPmdDiscriminatorMaker::Matching(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
StPmdClusterCollection* clusters1 =(StPmdClusterCollection*)cpv_det->cluster();
      Int_t nclust = clusters->Nclusters();
      Int_t nclust1 = clusters1->Nclusters();
	Double_t etapmd,phipmd,edeppmd;
	Double_t etacpv,phicpv,edepcpv;
	Double_t mcometa[1000],mcomphi[1000];
	Float_t etadiff,phidiff;
	Int_t m_common=0, icom=0;
        Float_t cutcluster=0;
	Bool_t mcpvdone;
        TIter next(clusters->Clusters());
            StPmdCluster *spmcl1;
        for(Int_t i=0; i<nclust ; i++)
               {
            spmcl1 = (StPmdCluster*)next();
            etapmd=spmcl1->CluEta();
            phipmd=spmcl1->CluPhi();
            edeppmd=spmcl1->CluEdep();
		if(edeppmd>=0.0000063)cutcluster++;
	        TIter nextcpv(clusters1->Clusters());
	           StPmdCluster *spmcl2;
		   Int_t no=0;
		   if(icom>0){
			   for(Int_t icom1=1; icom1<=icom; icom1++)
			   {
	   if((etapmd != mcometa[icom1]) && (phipmd != mcomphi[icom1]))
	   {
	mcpvdone=true;
	   }
			   }
			   }
	   if(true){
		 for(Int_t icpv=0; icpv<nclust1 ; icpv++)
		 {
			 spmcl2 = (StPmdCluster*)nextcpv();
                         etacpv=spmcl2->CluEta();
	                 phicpv=spmcl2->CluPhi();
		         edepcpv=spmcl2->CluEdep();
	etadiff=etapmd-etacpv;
	phidiff=phipmd-phicpv;
	mEtaPhi->Fill(etadiff,phidiff);
	if(fabs(etapmd-etacpv) <= mDeltaEta) 
			{
	if(fabs(phipmd-phicpv) <= mDeltaPhi) 
			{
				no++;
				if(no==1){
			m_common++;
			icom++;
			mcometa[icom]=etacpv;
			mcomphi[icom]=phicpv;
			spmcl1->setCluPID(8); //PID 8 set for photon
	            mEtaPhim->Fill(mcometa[icom],mcomphi[icom]);
			}
			}
			}// loop for cpv
              else 
               spmcl1->setCluPID(1); // PID 1 set for hadron
			}
	   }
   }//loop for pmd
mAboveEdep->Fill(cutcluster);
mCpvClusterm->Fill(nclust1,m_common);
mPmdClusterm->Fill(nclust,m_common);
}//loop for matching func

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
         Int_t PID=spmcl1->CluPID();
         Int_t EdepPID=spmcl1->CluEdepPID();
	   mClusterPID->Fill(Float_t(PID));
	   mClusterEdepPID->Fill(Float_t(EdepPID));
             }
}


































































































