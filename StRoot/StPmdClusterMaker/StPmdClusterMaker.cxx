/*************************************************
 *
 * $Id: StPmdClusterMaker.cxx,v 1.1 2002/08/27 12:09:08 subhasis Exp $
 * Author: Subhasis Chattopadhyay
 *************************************************
 *
 * Description: Base class for Pmd cluster Maker
 *
 *************************************************
 *
 * $Log: StPmdClusterMaker.cxx,v $
 * Revision 1.1  2002/08/27 12:09:08  subhasis
 * First version
 *
 *
 *************************************************/

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
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdClusterMaker.h"
#include "StPmdAbsClustering.h"
#include "StPmdClustering.h"
#include "StPmdUtil/StPmdDetector.h" //! will be obtained 
#include "StPmdUtil/StPmdModule.h" //! will be obtained 
#include "StPmdUtil/StPmdCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdClusterCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdCluster.h" //! will be obtained 

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
  mNcellPmdCluster = new TH1F("NcellPmd","No of Cellsper cls",50,-0.5,49.5);  
  mEtaPmdCluster  = new TH1F("EtaPmd","Eta Distribution",100,-4.,-2.);  
  mPhiPmdCluster  = new TH1F("PhiPmd","Phi Distribution",100,-3.14,3.14);
  mPhi2ModPmd  = new TH2F("Phi2ModPmd","Phi vs Mod",12,0.5,12.5,360,-3.14,3.14);
  mHitVscluster  = new TH2F("Pmd_hitvsClus","Hit vsclusPMD",50,0.5,50.5,50,0.5,50.5);

  mSmCpvCluster   = new TH1F("Smno_cpv","CPV SuperModule No",24,1.,24.);
  mEdepCpvCluster = new TH1F("EdepCpv","Cpv Energy deposited",600,0.,0.00004);  
  mNcellCpvCluster = new TH1F("NcellCpv","No of Cellsper cls (CPV)",50,-0.5,49.5);  
  mEtaCpvCluster  = new TH1F("EtaCpv","Cpv Eta Distribution",100,-4.,-2.);  
  mPhiCpvCluster  = new TH1F("PhiCpv","Cpv Phi Distribution",100,-3.14,3.14);
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
    
    FillHistograms(pmd_det,cpv_det);
  }
 return kStOK;
}//! loop for make ends here.
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
    if(pmd_det->module_hit(id)>0){
      Int_t nmh1=pmd_det->module_hit(id);
      tothitcpv+=nmh1;  
    }
  }
  
//  Float_t totedeppmd=AddEdep(pmd_det,cpv_det); //! total edep for each track added
  
  StPmdClusterCollection* clusters = (StPmdClusterCollection*)pmd_det->cluster();
  StPmdClusterCollection* cpvclusters = (StPmdClusterCollection*)cpv_det->cluster();
  
  Int_t nclust = clusters->Nclusters();   //! no of Pmd clusters
  
  Int_t nclust_cpv = cpvclusters->Nclusters(); //! no of Cpv clusters

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
      mSmPmdCluster->Fill(Float_t(mod));
      mEdepPmdCluster->Fill(edep);
      mSigmaPmdCluster->Fill(sigma);
      mNcellPmdCluster->Fill(Float_t(ncell));
      mEtaPmdCluster->Fill(eta);
      mPhiPmdCluster->Fill(phi);
				        }
  mHitVscluster->Fill(tothitpmd,Float_t(nclust));
  
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
      Int_t ncell=spmcl1->NumofMems();
      mSmCpvCluster->Fill(Float_t(mod));
      mEdepCpvCluster->Fill(edep);
      
      mNcellCpvCluster->Fill(Float_t(ncell));
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












