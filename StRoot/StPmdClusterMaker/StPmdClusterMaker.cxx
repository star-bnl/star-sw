/*************************************************
 *
 * $Id: StPmdClusterMaker.cxx,v 1.4 2003/09/02 17:58:48 perev Exp $
 * Author: Subhasis Chattopadhyay
 *************************************************
 *
 * Description: Base class for Pmd cluster Maker
 *
 *************************************************
 *
 * $Log: StPmdClusterMaker.cxx,v $
 * Revision 1.4  2003/09/02 17:58:48  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
 *
 *
 *************************************************/

#include<Stiostream.h>
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

#include "StBFChain.h"
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdClusterMaker.h"
#include "StPmdAbsClustering.h"
#include "StPmdClustering.h"
#include "StPmdUtil/StPmdDetector.h" //! will be obtained 
#include "StPmdUtil/StPmdModule.h" //! will be obtained 
#include "StPmdUtil/StPmdCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdClusterCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdCluster.h" //! will be obtained 


#include "StEventTypes.h"


ClassImp(StPmdClusterMaker)
  
  TDataSet *clusterIn;
StPmdCollection *cluster_hit;

//-------------------- 
StPmdClusterMaker::StPmdClusterMaker(const char *name):StMaker(name)
{
}
//-------------------

StPmdClusterMaker::~StPmdClusterMaker()
{
}
//---------------------
  Int_t StPmdClusterMaker::Init()
{
  bookHistograms();
  return StMaker::Init();
}
//--------------------------------------
void StPmdClusterMaker::bookHistograms()
{
  mSmPmdCluster   = new TH1F("Smno_pmd","SuperModule No",24,1.,24.);
  mEdepPmdCluster = new TH1F("EdepPmd","Energy deposited",600,0.,0.00004);  
  mSigmaPmdCluster = new TH1F("SigmaClusterPmd","Cluster Sigma",50,0.5,4.5);  
  mNcellPmdCluster = new TH1F("NcellPmd","No of Cells per cls",50,-0.5,49.5);  
  mEtaPmdCluster  = new TH1F("EtaPmd","Eta Distribution",100,-4.,-2.);  
  mPhiPmdCluster  = new TH1F("PhiPmd","Phi Distribution",100,-3.14,3.14);
  mPmdCluster  = new TH1F("PmdCluster"," NCluster in PMD",100,0,5000);
  mPhi2ModPmd  = new TH2F("Phi2ModPmd","Phi vs Mod",12,0.5,12.5,360,-3.14,3.14);
  mHitVscluster  = new TH2F("Pmd_hitvsClus","Hit vsclusPMD",50,0.5,50.5,50,0.5,50.5);
  mSigmaCpvCluster = new TH1F("SigmaClusterCpv","Cluster Sigma",50,0.5,4.5);  
  mSmCpvCluster   = new TH1F("Smno_cpv","CPV SuperModule No",24,1.,24.);
  mEdepCpvCluster = new TH1F("EdepCpv","Cpv Energy deposited",600,0.,0.00004);  
  mNcellCpvCluster = new TH1F("NcellCpv","No of Cellsper cls (CPV)",50,-0.5,49.5);  
  mEtaCpvCluster  = new TH1F("EtaCpv","Cpv Eta Distribution",100,-4.,-2.);  
  mPhiCpvCluster  = new TH1F("PhiCpv","Cpv Phi Distribution",100,-3.14,3.14);
  mCpvCluster  = new TH1F("CPVCluster"," NCluster in CPV",100,0,5000);
}
//--------------------------------
Int_t StPmdClusterMaker::Make() 
{
  clusterIn = GetDataSet("PmdSimulator"); //! getting data from StPmdSimulator
  cluster_hit = (StPmdCollection*)clusterIn->Find("PmdCollection");

  if(cluster_hit){
    StPmdDetector * pmd_det = cluster_hit->detector(Int_t(0));
    StPmdDetector * cpv_det = cluster_hit->detector(Int_t(1));
    
    Int_t choice=1; // Enter choice
    if(choice==1){
      StPmdClustering *clust1 = new StPmdClustering(pmd_det, cpv_det);
      if(clust1){
	clust1->findPmdClusters(); //! find Clustering for PMD

	clust1->findCpvClusters();  //! find Clustering for CPV

      }
      else{
	cout<<"clust1 not made"<<endl;
      }
    }
    FillStEvent(pmd_det,cpv_det);
    
    FillHistograms(pmd_det,cpv_det);
  }
 return kStOK;
}/*! loop for make ends here.*/
//--------------------------
void StPmdClusterMaker::Browse(TBrowser *b)
{
  TDataSet::Browse(b);
}
//---------------------------------------------------
void StPmdClusterMaker::FillHistograms(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  //! Get Hits
  Int_t tothitpmd=0;
  
  Int_t tothitcpv=0;
  for(Int_t id=1;id<=12;id++){
    if(pmd_det->module_hit(id)>0){
      Int_t nmh=pmd_det->module_hit(id);
      tothitpmd+=nmh;  
    }

    if(cpv_det->module_hit(id)>0){
      Int_t nmh1=pmd_det->module_hit(id);
      tothitcpv+=nmh1;  
    }
  }
  
//  Float_t totedeppmd=AddEdep(pmd_det,cpv_det); //! total edep for each track added
  
  StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  StPmdClusterCollection* cpvclusters = (StPmdClusterCollection*)cpv_det->cluster();
  
  Int_t nclust = clusters->Nclusters();   //! no of Pmd clusters
  
  Int_t nclust_cpv = cpvclusters->Nclusters(); //! no of Cpv clusters

  cout<<" TOTAL HIT PMD, CPV " <<tothitpmd<<" "<<tothitcpv<<endl;
  cout<<" NClust PMD, NClust CPV" <<" "<<nclust<<" "<<nclust_cpv<<endl;

  //! First Fill PMD
  TIter next(clusters->Clusters());

  StPmdCluster *spmcl1;
  for(Int_t i=0; i<nclust ; i++)
    {
      spmcl1 = (StPmdCluster*)next();
      Float_t eta=spmcl1->CluEta();
      Float_t phi=spmcl1->CluPhi();
      Float_t edep=spmcl1->CluEdep();
      Float_t sigma=spmcl1->CluSigma();
      Int_t mod=spmcl1->Module();
      Int_t ncell=spmcl1->NumofMems();
      //      cout<<"Eta,Phi,Edep,Sigma,Ncell "<<eta<<" "<<phi<<" "<<edep<<" "<<sigma<<" "<<ncell<<endl;

      TIter next1(spmcl1->HitCollection());
      StPmdHit  *hit;
      //! loop over each cluster for getting hits
      for(Int_t im=0;im<ncell;im++){
	hit = (StPmdHit*)next1();

	if(hit){
	  //	  Int_t row = hit->Row();
	  //	  Int_t super = hit->Gsuper();
	  //	  Int_t col = hit->Column();

	}
      }
  
      mSmPmdCluster->Fill(Float_t(mod));
      mEdepPmdCluster->Fill(edep);
      mSigmaPmdCluster->Fill(sigma);
      mNcellPmdCluster->Fill(Float_t(ncell));
      mEtaPmdCluster->Fill(eta);
      mPhiPmdCluster->Fill(phi);
    }
  mHitVscluster->Fill(tothitpmd,Float_t(nclust));
  mPmdCluster->Fill(nclust);
  mCpvCluster->Fill(nclust_cpv);  
  //! NOW for Fill CPV
  TIter nextcpv(cpvclusters->Clusters());
  StPmdCluster *spmcl2;
  for(Int_t i=0; i<nclust_cpv ; i++)
    {
      spmcl2 = (StPmdCluster*)nextcpv();
      Float_t eta=spmcl2->CluEta();
      Float_t phi=spmcl2->CluPhi();
      Float_t edep=spmcl2->CluEdep();
      Int_t mod=spmcl2->Module();
      Float_t sigma=spmcl2->CluSigma();
      Int_t ncell1=spmcl2->NumofMems();

      TIter next1(spmcl2->HitCollection());
      StPmdHit  *hit;
      //! loop over each cluster for getting hits
      for(Int_t im=0;im<ncell1;im++){
	hit = (StPmdHit*)next1();

	if(hit){
	  //	  Int_t row = hit->Row();
	  //	  Int_t super = hit->Gsuper();
	  //	  Int_t col = hit->Column();

	}
      }
      mSmCpvCluster->Fill(Float_t(mod));
      mEdepCpvCluster->Fill(edep);
      mSigmaCpvCluster->Fill(sigma);
      mNcellCpvCluster->Fill(Float_t(ncell1));
      mEtaCpvCluster->Fill(eta);
      mPhiCpvCluster->Fill(phi);
    }
  
}

//! used for adding total edep of each track
Float_t StPmdClusterMaker::AddEdep(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  //! Get Hits
  
  Float_t totedeppmd = 0;
  for(Int_t id=1;id<=12;id++){
    if(pmd_det->module_hit(id)>0){
      Int_t nmh=pmd_det->module_hit(id);
      
      StPmdModule * pmd_mod=pmd_det->module(id);  //! getting module(id)
      if(nmh>0){
	TIter next(pmd_mod->Hits());    //! getting hits
	StPmdHit *spmcl;
	for(Int_t im=0; im<nmh; im++)
	  {
	    spmcl = (StPmdHit*)next();
	    if(spmcl){
	      Float_t edep=spmcl->Edep();   //! edep
	      totedeppmd+=edep;
	    }
	  }
      }    
    }
  }
  return totedeppmd;
}


//---------------------------------------------------
void StPmdClusterMaker::FillStEvent(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
	cout<<"filling SytEvent in ClusterMaker"<<endl;
// Get StEvent
	StPhmdCollection * PmdCollection; 
	StEvent *currevent = (StEvent*)GetInputDS("StEvent");
	if(!currevent){
		cout<<"ClusterMaker **, No StEvent Pointer "<<endl;
	}

        if(currevent)PmdCollection= currevent->phmdCollection();
	  if(PmdCollection){
		  StPhmdDetector* evtdet0 = PmdCollection->detector(StDetectorId(kPhmdId));
	    StPhmdDetector* evtdet1 = PmdCollection->detector(StDetectorId(kPhmdCpvId));
	       // add clustercollection
	    StPhmdClusterCollection* cluscollpmd = new StPhmdClusterCollection();
	     cluscollpmd->setClusterFinderId(1);
	     cluscollpmd->setClusterFinderParamVersion(1);
//CPV
	     StPhmdClusterCollection* cluscollcpv = new StPhmdClusterCollection();
	     cluscollcpv->setClusterFinderId(1);
	     cluscollcpv->setClusterFinderParamVersion(1);
  
	     StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  StPmdClusterCollection* cpvclusters = (StPmdClusterCollection*)cpv_det->cluster();
  
  Int_t nclust = clusters->Nclusters();   //! no of Pmd clusters
  
  Int_t nclust_cpv = cpvclusters->Nclusters(); //! no of Cpv clusters

  //! First Fill PMD
	  if(evtdet0){
	     evtdet0->setCluster(cluscollpmd);
  TIter next(clusters->Clusters());
  StPmdCluster *spmcl1;
  for(Int_t i=0; i<nclust ; i++)
    {
      spmcl1 = (StPmdCluster*)next();
      Float_t eta=spmcl1->CluEta();
      Float_t phi=spmcl1->CluPhi();
      Float_t edep=spmcl1->CluEdep();
      Float_t sigma=spmcl1->CluSigma();
      Int_t mod=spmcl1->Module();
      Int_t ncell=spmcl1->NumofMems();
      // Filling PmdCluster for StEvent
               StPhmdCluster *pcls = new StPhmdCluster();
               pcls->setModule(mod);     
               pcls->setNumberOfCells(ncell); 
               pcls->setEta(eta);
               pcls->setPhi(phi);
               pcls->setEnergy(edep);
               pcls->setSigma(sigma);
               cluscollpmd->addCluster(pcls);
//	       cout<<"PMDclust added  "<<endl;
    }       
	  } 
  //! NOW for Fill CPV
 if(evtdet1){
	     evtdet1->setCluster(cluscollcpv);
  TIter nextcpv(cpvclusters->Clusters());
  StPmdCluster *spmcl2;
  for(Int_t i=0; i<nclust_cpv ; i++)
    {
      spmcl2 = (StPmdCluster*)nextcpv();
      Float_t eta=spmcl2->CluEta();
      Float_t phi=spmcl2->CluPhi();
      Float_t edep=spmcl2->CluEdep();
      Float_t sigma=spmcl2->CluSigma();
      Int_t mod=spmcl2->Module();
      Int_t ncell=spmcl2->NumofMems();
      // Filling PmdCluster for StEvent
               StPhmdCluster *pcls = new StPhmdCluster();
               pcls->setModule(mod);     
               pcls->setNumberOfCells(ncell); 
               pcls->setEta(eta);
               pcls->setPhi(phi);
               pcls->setEnergy(edep);
               pcls->setSigma(sigma);
               cluscollcpv->addCluster(pcls);
    }       
}
}
	cout<<"filled SytEvent in ClusterMaker"<<endl;
}








