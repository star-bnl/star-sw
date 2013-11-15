/*************************************************
 *
 * $Id: StPmdClusterMaker.cxx,v 1.22 2010/05/29 00:47:10 rashmi Exp $
 * Author: Subhasis Chattopadhyay
 *************************************************
 *
 * Description: Base class for Pmd cluster Maker
 *
 *************************************************
 *
 * $Log: StPmdClusterMaker.cxx,v $
 * Revision 1.22  2010/05/29 00:47:10  rashmi
 * Added a call to new clustering routines in StPmdClustering
 *
 * Revision 1.20  2007/11/02 10:59:30  rashmi
 * Applying hitcalibration; eta,phi wrt primary vertex
 *
 * Revision 1.19  2007/10/26 18:14:18  rashmi
 * fixed some warnings
 *
 * Revision 1.18  2007/08/31 10:53:30  rashmi
 * Included ReadCalibration to read PMD_MIP value from DB;Setting cutoff in StPmdClusteringas 15%PMD_MIP
 *
 * Revision 1.17  2007/04/26 04:10:34  perev
 * Remove StBFChain dependency
 *
 * Revision 1.16  2006/02/16 08:35:26  subhasis
 * fillstevent: mod-1 fixed for CPV
 *
 * Revision 1.15  2004/11/15 23:35:47  subhasis
 * Refs in centroidCal initialised to solve valgrind error
 *
 * Revision 1.13  2004/09/22 19:24:55  perev
 * Leak fixed + mess with i,j indexes
 *
 * Revision 1.11  2004/09/03 14:31:53  subhasis
 * OptHist introduced
 *
 * Revision 1.10  2004/07/26 12:01:18  subhasis
 * sigmaL, sigmaS stored in one place till StPhmdCluster.h modified
 *
 * Revision 1.9  2004/07/09 09:15:54  subhasis
 * numbering convention starts from 0 everywhere for filling StEvent
 *
 * Revision 1.7  2004/06/24 13:46:52  subhasis
 * several changes in clustering code
 *
 * 2004/06/24: findCpvCluster() function removed,
 * findPmdCluster() has been called for both the planes.
 * New clustering algorithm has been implemented.
 * Presently we are filling SigmaL only(not SigmaS)
 * in StEvent with function name sigma() but in 
 * TDataSet both SigmaL and SigmaS are present : Dipak
 * 
 * Revision 1.6  2004/03/23 05:18:54  subhasis
 * refclust changed to have correct sigma/ncell
 *
 * Revision 1.4  2003/09/02 17:58:48  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/05/14 10:49:12  subhasis
 * CPV clustering added
 *
 *
 *************************************************/

#include<Stiostream.h>
#include "TStopwatch.h"
#include<assert.h>
#include<math.h>
#include"TROOT.h"
#include<TRandom.h>
#include<TBrowser.h>
#include<TPad.h>
#include<StMessMgr.h>
#include<TFile.h>

#include "StPmdUtil/StPmdGeom.h"
#include "StPmdClusterMaker.h"
#include "StPmdAbsClustering.h"
#include "StPmdClustering.h"
#include "StPmdUtil/StPmdDetector.h" //! will be obtained 
#include "StPmdUtil/StPmdModule.h" //! will be obtained 
#include "StPmdUtil/StPmdCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdClusterCollection.h" //! will be obtained 
#include "StPmdUtil/StPmdCluster.h" //! will be obtained 

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"
//ofstream clout("cluster_out.dat");

#include "StEventTypes.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "SystemOfUnits.h"

ClassImp(StPmdClusterMaker)
  
  TDataSet *clusterIn;
StPmdCollection *cluster_hit;
Float_t xv,yv,zv;
//-------------------- 
StPmdClusterMaker::StPmdClusterMaker(const char *name):StMaker(name)
{
  mOptHist=kFALSE;
  
  // To be put false if we are working with simulated data
  // OR
  // We are working with data without calibration constants in the DB
  mOptCalibrate=kFALSE;

  // For real data, simulation flag has to be off
  // This is to set the adccutoff to the noiseless limit of 0.4 later
  mOptSimulate=kFALSE;
  //mOptSimulate=kTRUE;
  
  // PMD analysis with refined clustering on
  //  mOptRefineCluster=kTRUE;
  // PMD analysis with refined clustering off
  mOptRefineCluster=kFALSE;
  
  cout<<" Default options: mOptCalibrate = "<<mOptCalibrate<<" mOptRefineCluster = "<<mOptRefineCluster<<" mOptSimulate="<<mOptSimulate<<endl;
  PMD_MIP=0.0;
  
}
//-------------------

StPmdClusterMaker::~StPmdClusterMaker()
{
}
//---------------------
Int_t StPmdClusterMaker::Init()
{
  
  if(mOptHist)bookHistograms();
  
  return StMaker::Init();
}
//--------------------------------------
                                                                         
Int_t StPmdClusterMaker::InitRun(Int_t runnr) {
                                                                         
  if(Debug())cout<<"StPmdClusterMaker::InitRun with run "<<runnr<<endl;

  if(mOptCalibrate==kTRUE){
    ReadCalibrationsConst();
  }else{
    cout<<"Not reading the calibration Constants"<<endl;
  }
                                                                         
  return StMaker::InitRun(runnr);
                                                                         
}

//--------------------------------------
void StPmdClusterMaker::bookHistograms()
{
  mNclust= new TH1F("Nclust","Nclust (no cut)",100,-0.5,100.5);
  mNclust1= new TH1F("Nclust1","Nclust (1MIP cut)",100,-0.5,100.5);
  mNclust2= new TH1F("Nclust2","Nclust (2MIP cut)",100,-0.5,100.5);
  mNclust3= new TH1F("Nclust3","Nclust (3MIP cut)",100,-0.5,100.5);
  
  mSmPmdCluster   = new TH1F("Smno_pmd","SuperModule No",24,1.,24.);
  mEdepPmdCluster = new TH1F("EdepPmd","Energy deposited",10000,0.,10000);  
  mSigmaLPmdCluster = new TH1F("SigmaLClusterPmd","Cluster SigmaL",50,0.5,4.5);  
  mSigmaSPmdCluster = new TH1F("SigmaSClusterPmd","Cluster SigmaS",50,0.5,4.5);  
  mNcellPmdCluster = new TH1F("NcellPmd","No of Cells per cls",50,-0.5,49.5);  
  mEtaPmdCluster  = new TH1F("EtaPmd","Eta Distribution",100,-4.,-2.);  
  mPhiPmdCluster  = new TH1F("PhiPmd","Phi Distribution",100,-3.14,3.14);
  mPmdCluster  = new TH1F("PmdCluster"," NCluster in PMD",100,0,5000);
  mPhi2ModPmd  = new TH2F("Phi2ModPmd","Phi vs Mod",12,0.5,12.5,360,-3.14,3.14);
  mHitVscluster  = new TH2F("Pmd_hitvsClus","Hit vsclusPMD",50,0.5,50.5,50,0.5,50.5);
  mSigmaLCpvCluster = new TH1F("SigmaLClusterCpv","Cluster SigmaL",50,0.5,4.5);  
  mSigmaSCpvCluster = new TH1F("SigmaSClusterCpv","Cluster SigmaS",50,0.5,4.5);  
  mSmCpvCluster   = new TH1F("Smno_cpv","CPV SuperModule No",24,1.,24.);
  mEdepCpvCluster = new TH1F("EdepCpv","Cpv Energy deposited",5000,0.,5000.);  
  mNcellCpvCluster = new TH1F("NcellCpv","No of Cellsper cls (CPV)",50,-0.5,49.5);  
  mEtaCpvCluster  = new TH1F("EtaCpv","Cpv Eta Distribution",100,-4.,-2.);  
  mPhiCpvCluster  = new TH1F("PhiCpv","Cpv Phi Distribution",100,-3.14,3.14);
  mCpvCluster  = new TH1F("CPVCluster"," NCluster in CPV",100,0,5000);
  mXYCpvCluster = new TH2F("CPV2D" ,"CPV Cluster 2D", 400,-200.,200.,400,-200.,200.);
  mXYPmdCluster = new TH2F("PMD2D" ,"PMD Cluster 2D", 400,-200.,200.,400,-200.,200.);
}
//--------------------------------
Int_t StPmdClusterMaker::Make() 
{
	TStopwatch clock;
	  clock.Start();
  clusterIn = GetDataSet("PmdSimulator"); //! getting data from StPmdSimulator
  if(!clusterIn){
    clusterIn = GetDataSet("pmdReader"); //! getting data from StPmdReader
  }
  if(!clusterIn){
    cout<<" No Hit_dataset found, return "<<endl;
    return kStWarn;
  }
  cluster_hit = (StPmdCollection*)clusterIn->Find("PmdCollection");
  
  if(!cluster_hit){
    cout<<" No PmdCollection found, return "<<endl;
    return kStWarn;
  }
  
  
  if(cluster_hit)
    {
      StPmdDetector * cpv_det = cluster_hit->detector(Int_t(0)); //CPV = 0 in PmdCollection
      StPmdDetector * pmd_det = cluster_hit->detector(Int_t(1)); //PMD = 1 in PmdCollection

      // Get the multiplicity of hits on cpv and pmd plane
      
      Int_t cpv_hits = cpv_det->numberOfHits();
      Int_t pre_hits = pmd_det->numberOfHits();
      cout<<" Number of hits on CPV = "<<cpv_hits<<endl;
      cout<<" Number of hits on PMD = "<<pre_hits<<endl;
      if((cpv_hits + pre_hits)>15000){
	cout<<" The number of hits on PMD are too large"<<endl;
	cout<<" The existing limit is 15000 hit on CPV+Pre"<<endl;
	return kStWarn;
      }

     // StPmdClustering *clust1; // added for getting pointer to StPmdClustering
      Int_t choice=1; // Enter choice (it is put as 1, but variable was kept to adopt differt
                      // choice of algo.
      if(choice==1){
      if(cpv_det && pmd_det){
	StPmdClustering *clust1 = new StPmdClustering(pmd_det, cpv_det); //instantiates clustering
	// If the PMD_MIP is finite (non-zero)
        if(PMD_MIP>0){
	  //	  clout<<"PMD_MIP_MPV="<< PMD_MIP<<" in make"<<endl;
          Double_t adccutoff = (Double_t)(PMD_MIP*0.15);
	  //	  clout<<" adccutoff="<<adccutoff<<endl;
          clust1->SetAdcCutOff(adccutoff);
        }else{
	  if(mOptCalibrate==kFALSE) {
	    clust1->SetAdcCutOff(7.0);
	    clust1->SetOptCalibrate(kFALSE);
	  }
        }
	if(mOptSimulate==kTRUE) {
	  cout<<" Simulationflag is ON so the cutoff is set to 0.4"<<endl;
	  clust1->SetAdcCutOff(0.4);
	  clust1->SetOptSimulate(kTRUE);
	}
	if(mOptRefineCluster==kFALSE) {
	  cout<<" Refined clustering is put off. (Default = ON)"<<endl;
	  clust1->SetOptRefineCluster(kFALSE);
	}
	
  	// Getting the vertex info to set it in StPmdClustering routine.
	StEvent *currevent = (StEvent*)GetInputDS("StEvent");
	if(!currevent){
	  cout<<"ClusterMaker **, No StEvent Pointer "<<endl;
	}
  	StPrimaryVertex* Vertex=currevent->primaryVertex();
	//  	if(!Vertex) return kStError;
  	if(Vertex){
	  StThreeVectorF v = Vertex->position();
	  //	  cout<<"In StPmdClusterMaker: vertex="<<v.x()<<","<<v.y()<<","<<v.z()<<endl;
	  clust1->SetVertexPos(v);
	}
	
	if(clust1)
	  {
	    for(Int_t d=0;d<2;d++)  // Loop over detectors
	      {
		StPmdDetector *det = cluster_hit->detector(d); //PMD = 1 and 0 for CPV in PmdCollection
		// This uses the new clustering routine
		clust1->findPmdClusters2(det); //! find Clustering 
		// This uses the old clustering routine
		//clust1->findPmdClusters(det); //! find Clustering 
	      } //for loop 'd'
	  }
	else
	  {
	    cout<<"clust1 not made"<<endl;
	    return kStOK;
	  }
	FillStEvent(pmd_det,cpv_det);
	cout<<" Number of pmd clusters="<<((StPmdClusterCollection*)pmd_det->cluster())->Nclusters()<<endl;
	cout<<" Number of cpv clusters="<<((StPmdClusterCollection*)cpv_det->cluster())->Nclusters()<<endl;
	
	//      cout<<"stevent filled , to go hist "<<endl;
	if(mOptHist)FillHistograms(pmd_det,cpv_det);
	//      cout<<"hist filled  "<<endl;
      }
      } // if loop 'choice'
    }
  //   clock.Stop();
  //     cout <<"Time to run StPmdClusterEMaker::Make() real = "<<clock.RealTime()<<"  cpu = "<<clock.CpuTime()<<" \n";
  //     cout <<"*******************************************************************************************\n\n\n";


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
      Int_t nmh1=cpv_det->module_hit(id);
      tothitcpv+=nmh1;  
    }
  }
  
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
      Float_t sigmaL=spmcl1->CluSigmaL();
      Float_t sigmaS=spmcl1->CluSigmaS();
      Int_t mod=spmcl1->Module();
      Float_t ncell=spmcl1->NumofMems();
      Float_t xclu = spmcl1->CluX();
      Float_t yclu = spmcl1->CluY();

      mSmPmdCluster->Fill(Float_t(mod));
      mEdepPmdCluster->Fill(edep);    // In keV
      mSigmaLPmdCluster->Fill(sigmaL);
      mSigmaSPmdCluster->Fill(sigmaS);
      mNcellPmdCluster->Fill(ncell);
      mEtaPmdCluster->Fill(eta);
      mPhiPmdCluster->Fill(phi);
      mXYPmdCluster->Fill(xclu,yclu);
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
      Float_t sigmaL=spmcl2->CluSigmaL();
      Float_t sigmaS=spmcl2->CluSigmaS();
      Float_t ncell1=spmcl2->NumofMems();
      Float_t xclu = spmcl2->CluX();
      Float_t yclu = spmcl2->CluY();
      
      
      mSmCpvCluster->Fill(Float_t(mod));
      mEdepCpvCluster->Fill(edep); //In keV
      mSigmaLCpvCluster->Fill(sigmaL);
      mSigmaSCpvCluster->Fill(sigmaS);
      mNcellCpvCluster->Fill(ncell1);
      mEtaCpvCluster->Fill(eta);
      mPhiCpvCluster->Fill(phi);
      mXYCpvCluster->Fill(xclu,yclu);
    }
  
}

Int_t StPmdClusterMaker::Finish()
{
  return StMaker::Finish();
}



//---------------------------------------------------
void StPmdClusterMaker::FillStEvent(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  cout<<"Filling StEvent in ClusterMaker"<<endl;
  // Get StEvent
  StPhmdCollection * PmdCollection=NULL; 
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  if(!currevent){
    cout<<"ClusterMaker **, No StEvent Pointer "<<endl;
  }
  
  if(currevent)PmdCollection= currevent->phmdCollection();
  if(PmdCollection)
    {
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
      if(evtdet0)
	{
	  evtdet0->setCluster(cluscollpmd);
	  TIter next(clusters->Clusters());
	  StPmdCluster *spmcl1;
	  Int_t clustmip1=0;
	  Int_t clustmip2=0;
	  Int_t clustmip3=0;
	  for(Int_t i=0; i<nclust ; i++)
	    {
	      spmcl1 = (StPmdCluster*)next();
	      Float_t eta=spmcl1->CluEta();
	      Float_t phi=spmcl1->CluPhi();
	      Float_t edep=spmcl1->CluEdep();
	      Float_t sigmaL=spmcl1->CluSigmaL();
	      Float_t sigmaS=spmcl1->CluSigmaS();
	      Int_t tempL=Int_t(sigmaL*1000);
	      Int_t tempS=Int_t(sigmaS*1000);
	      Int_t SigInt=tempL*10000+tempS;
	      if(edep>2.5)clustmip1++;
	      if(edep>5.)clustmip2++;
	      if(edep>7.5)clustmip3++;

	      Int_t mod=spmcl1->Module();
	      Float_t ncell=spmcl1->NumofMems();
	      //	      if(ncell>2){ cout<<" big cluster ="<<ncell<<endl;}
	      // Filling PmdCluster info in StEvent
	      StPhmdCluster *pcls = new StPhmdCluster();
	      //pcls->setModule(mod);           //! Supermodule
	      // supmod filled as 0-11, earlier it was 1-12
	      pcls->setModule(mod-1);           //! Supermodule
	      pcls->setNumberOfCells(Int_t(ncell));  //! # of Cells( Integer type) in a cluster
	      // Commented temporarily because of problem in StEvent
	      // pcls->setNumberOfCells(ncell);  //! # of Cells in a cluster
	      pcls->setEta(eta);              //! Cluster Eta
	      pcls->setPhi(phi);              //! Cluster Phi
	      pcls->setEnergy(edep);          //! Cluster Edep
	      pcls->setSigma((Float_t)SigInt);       //! Filling Sigma Large
	      // SigmaL and sigmaS are put together are stored due tolack of 
	      // spce for two of them
	      cluscollpmd->addCluster(pcls);  // Adding to ClusterCollection in StEvent
	    }       
      if(mOptHist){
      mNclust->Fill(nclust);
      mNclust1->Fill(clustmip1);
      mNclust2->Fill(clustmip2);
      mNclust3->Fill(clustmip3);
      }
	} 

      //! NOW for Fill CPV
      if(evtdet1)
	{
	  evtdet1->setCluster(cluscollcpv);
	  TIter nextcpv(cpvclusters->Clusters());
	  StPmdCluster *spmcl2;
	  for(Int_t i=0; i<nclust_cpv ; i++)
	    {
	      spmcl2 = (StPmdCluster*)nextcpv();
	      Float_t eta=spmcl2->CluEta();
	      Float_t phi=spmcl2->CluPhi();
	      Float_t edep=spmcl2->CluEdep();
	      Float_t sigmaL=spmcl2->CluSigmaL();
	      Float_t sigmaS=spmcl2->CluSigmaS();
	      Int_t tempL=Int_t(sigmaL*1000);
	      Int_t tempS=Int_t(sigmaS*1000);
	      Int_t SigInt=tempL*10000+tempS;
	      Int_t mod=spmcl2->Module();
	      Float_t ncell=spmcl2->NumofMems();
	      
	      // Filling PmdCluster for StEvent
	      StPhmdCluster *pcls = new StPhmdCluster();
	      // supmod filled as 0-11, earlier it was 1-12 (earlier version it was a mistake, it was changed for pmd not for cpv)
	      pcls->setModule(mod-1);            //! Supermodule
	      pcls->setNumberOfCells(Int_t(ncell));  //! # of Cells( Integer type) in a cluster
	      // Commented temporarily because of problem in StEvent
	      //  pcls->setNumberOfCells(ncell);   //! # of Cells in a cluster
	      pcls->setEta(eta);               //! Cluster Eta
	      pcls->setPhi(phi);               //! Cluster Phi
	      pcls->setEnergy(edep);           //! Cluster Edep
	      pcls->setSigma(Float_t(SigInt));           //! Filling Sigma Large
	      cluscollcpv->addCluster(pcls);   // Adding to ClusterCollection in StEvent
	    }       
	}
    }
  cout<<"Filled PMD StEvent in ClusterMaker"<<endl;
}


//---------------------------------------------------------------------

Bool_t StPmdClusterMaker::ReadCalibrationsConst()
{
  if(Debug())cout<<" I AM IN READCALIB "<<endl;
                                                                         
  StDbManager* mgr=StDbManager::Instance();
  StDbConfigNode* node=mgr->initConfig("Calibrations_pmd");
  node->setVersion("SMChain");
                                                                         
  mDb=NULL;
  TString DbName = "Calibrations/pmd";
  mDb=GetInputDB(DbName.Data());
  //  clout<<"after mDB"<<endl;
  if(!mDb) return kFALSE;
  //clout<<"after !mDb"<<mDb->GetTimeStamp()<<endl;
                                                                         
  for(Int_t ism=0;ism<24;ism++){
    for(Int_t chain=0;chain<48;chain++){
      SM_chain_factor[ism][chain]=0.;
    }
  }
                                                                         
  St_pmdSMChain_GNF * tab = (St_pmdSMChain_GNF*) mDb->Find("pmdSMChain_GNF");
  if (!tab) {
    cout<<"No pmdSMChain_GNF DBTable. Stopping."<<endl;
    return kFALSE;
  }
                                                                         
  //  clout<<"Got SmChain tables"<<endl;
                                                                         
  //Getting the PMD_MIP values
  pmdSMChain_GNF_st* smchain = tab->GetTable(64);
  PMD_MIP = smchain->mpv_factor;
  //  clout<<"sm="<<smchain->sm<<" chain="<<smchain->chain<<" PMD_MIP_Mean="<<smchain->mean_factor<<" PMD_MIP_MPV="<<smchain->mpv_factor<<endl;
                                                                         
  return kTRUE;
                                                                         
}

