//
// $Id: StEpcMaker.cxx,v 1.6 2000/08/29 20:40:06 subhasis Exp $
// $Log: StEpcMaker.cxx,v $
// Revision 1.6  2000/08/29 20:40:06  subhasis
// Modified to accept input from StEvent and writing output to StEvent for Emc
//
// Revision 1.3  2000/05/16 21:48:32  subhasis
//  new checks for events with no clusters
//
// Revision 1.2  2000/05/15 21:53:57  subhasis
// initialversion
//
// Revision 1.1  2000/05/15 21:18:32  subhasis
// initial version
//
// EMC-track Match Maker
//
//
// Authors: Subhasis Chattopadhyay , February 2000.
//    

//////////////////////////////////////////////////////////////////////////
//                                                             
// StEpcMaker class  
//                                                             
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <math.h>
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "SystemOfUnits.h"
#include "St_emc_Maker/StEmcGeom.h"
#include <stdlib.h>
#include <string.h>
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StPhysicalHelixD.hh"
#include "TMath.h"
#include "StDetectorDefinitions.h"
#include "Stypes.h"
#include "math_constants.h"
#include "StEpcMaker.h"

// For StEvent
#include "StEventTypes.h"
#include "St_ObjectSet.h"
#include "StEvent.h" 
#include "StContainers.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StEmcClusterCollection.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEnumerations.h"

StTrackVec TrackVecForEmc;

                                                      
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

// declaring cernlib routine (mathlib, H301) assndx to be used for matching.
#include "StarCallf77.h"
#define    assndx  F77_NAME(assndx,ASSNDX)
extern "C" {void type_of_call assndx ( Int_t &, Float_t (*)[], Int_t &, Int_t &, Int_t &,Int_t *, Float_t &,Int_t (*)[],Int_t &); }


ClassImp(StEpcMaker)

const TString detname[] = {"Bemc", "Bsmde", "Bsmdp"};
//_____________________________________________________________________________
StEpcMaker::StEpcMaker(const char *name):StMaker(name){
  //  drawinit=kFALSE;
}
//_____________________________________________________________________________
StEpcMaker::~StEpcMaker(){
}
//________________________________________________________________________
Int_t StEpcMaker::Init(){
//Making QA histgrams
  // for points

  m_point_energy= new TH1F(" Point Energy "," Point energy ",100,0.,10.);
  m_point_eta= new TH1F(" Point Eta "," Point Eta ",100,-1.,1.);
  m_point_phi= new TH1F(" Point Phi "," Point Phi ",100,-4.,4.);
  m_point_sigeta= new TH1F(" Point SigEta "," Point SigEta ",100,-0.5,0.5);
  m_point_sigphi= new TH1F(" Point SigPhi "," Point SigPhi ",100,-0.5,0.5);
  m_point_deleta= new TH1F(" Point DelEta "," Point DelEta ",100,-0.1,0.1);
  m_point_delphi= new TH1F(" Point DelPhi "," Point DelPhi ",100,-0.1,0.1);
  m_point_trmom= new TH1F(" Point TrMom "," Point TrMom ",100,0.,10.);
  m_point_flag= new TH1F(" Point Flag "," Point Flag ",5,0.5,5.5);
  m_emc_points= new TH1F(" Points Multiplicity "," Points ",50,0.5,200.5);

  return StMaker::Init();
}  
//_________________________________________________________________________
Int_t StEpcMaker::Make(){
cout<<" StEpcMaker Make() **"<<endl;
// ptr initialization
mTheEmcCollection=0;                                                            

  if (!m_DataSet->GetList()){   //if DataSet is empty, create object and fill it
  //
  //Getting tracks from StEvent
  // grab StEvent
  cout << "about to Obtain Stevent" << endl;
  mEvent = (StEvent *) GetInputDS("StEvent");
 
  if (!mEvent) {
    cout << "No StEvent! Can not continue. " << endl;
    return kStOK; // If no event, we're done
  }
  cout<<" StEpcMaker** , StEvent found**"<<endl; 
  //First Find if EmcCollection exists

 if(mEvent){
   cout<<"Epc::StEvent found **"<<endl;
        mTheEmcCollection = mEvent->emcCollection();
  }
 if(!mTheEmcCollection){
   cout<<" EPC:: No EmcCollection, Cannot continue**"<<endl;
   return kStOK;
 }

   StDetectorId EmcId;
   StEmcDetector* EmcDet;
   StEmcClusterCollection* cluscoll;
   StEmcClusterCollection* Bemccluster;
   StEmcClusterCollection* Bprscluster;
   StEmcClusterCollection* Bsmdecluster;
   StEmcClusterCollection* Bsmdpcluster;

   if(mTheEmcCollection){
   cout<<" StEvent EmcCollection exists**"<<endl;

   for(Int_t idet=0;idet<4;idet++){
     EmcId = static_cast<StDetectorId>(idet+kBarrelEmcTowerId); 
     EmcDet =mTheEmcCollection->detector(EmcId);
     if(EmcDet){
     cluscoll = EmcDet->cluster();
     if(cluscoll){
       //     Int_t Nclusters=cluscoll->numberOfClusters();
     //     if(Nclusters>0){
     //     const StSPtrVecEmcCluster& emcclusters= cluscoll->clusters();
     //     cout<<" Clusters vector found, size **"<<emcclusters.size()<<endl;
     if(idet==0) Bemccluster=(StEmcClusterCollection*)cluscoll;
     if(idet==1) Bprscluster=(StEmcClusterCollection*)cluscoll;
     if(idet==2) Bsmdecluster=(StEmcClusterCollection*)cluscoll;
     if(idet==3) Bsmdpcluster=(StEmcClusterCollection*)cluscoll;

     }
     }
     }
   } 

   //-------------------
   //Tracks from StEvent
   //------------------- 

  StSPtrVecTrackNode& theTrackNodes = mEvent->trackNodes();
  cout<<" StEpcMaker:: Node size **"<<theTrackNodes.size()<<endl;

    int allGlobals = 0;
    int goodGlobals = 0;                                                       

  StTrack *track;
  //Initialize container for track

	 TrackVecForEmc.clear();
	 //

  for (size_t nodeIndex=0; nodeIndex<theTrackNodes.size(); nodeIndex++) {

     size_t numberOfTracksInNode =  theTrackNodes[nodeIndex]->entries(global);

       for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++)        {
            track =theTrackNodes[nodeIndex]->track(global,trackIndex);
        if (track) allGlobals++;
        if (accept(track)){
         TrackVecForEmc.push_back(track);
          goodGlobals++;
	}                                      
	  }
  }

  StTrackVec& TrackToFit=TrackVecForEmc;

 
 //******Creating StPointCollection and calling findPoints
	StPointCollection *point = new StPointCollection("point");
	m_DataSet->Add(point);

 if(point->findEmcPoints(Bemccluster,Bprscluster,Bsmdecluster,Bsmdpcluster,TrackToFit)!=kStOK)
 { return kStErr;}
else
 {
       cout<<" findEmcPoint called"<<endl;
 }

    MakeHistograms(); // Fill QA histgrams
   //------------------------------------------
   // WRITING IN EMCCOLLECTION IN STEVENT
   //------------------------------------------

  //Search for StEvent pointer , where EmcCollection is to be added
   //  mEvent=(StEvent *) GetInputDS("StEvent");
   //  if(mEvent){
   //      cout<<"Epc::StEvent found **"<<endl;
   //   if(!mTheEmcCollection){
   //        mTheEmcCollection = mEvent->emcCollection();
   //   }
   //  }
   //else{
   //    cout<<" *** Epc:: StEvent Structure does not exist***"<<endl;
   //    cout<<" *** Epc:: Try make one yourself***"<<endl;
   //    mEvent= new StEvent();
   //  }
   //   if(mTheEmcCollection){
   //   cout<<" StEvent EmcCollection exists**"<<endl;
   //   }
   cout<<" Epc***  Filling StEvent **"<<endl;
  Int_t fill = fillStEvent();
  if(fill != kStOK){cout<< "StEvent filling is not O.k"<<endl;}
  }
return kStOK;
}
//_________________________________________________________________________

void StEpcMaker::MakeHistograms()
{
  if (!m_DataSet) return;
  St_DataSetIter itr(m_DataSet);
  StPointCollection *cluster = 0;
  StPointCollection dummy;
  TString tit = dummy.GetTitle(); 
      cluster = (StPointCollection*)itr();
          if(cluster !=0){
          if(cluster->GetTitle() == tit){
            Int_t n = cluster->NPoints();
	    m_emc_points->Fill(Float_t(n));
            if(n>0){
              TIter next(cluster->Points());
              StPi0Candidate *cl;
            for(Int_t i=0;i<n;i++){
                  cl=(StPi0Candidate*)next();
                Float_t eta=cl->Eta();
                Float_t phi=cl->Phi();
                Float_t energy=cl->Energy();
                Float_t sigmaeta=cl->SigmaEta();
                Float_t sigmaphi=cl->SigmaPhi();
                Float_t trackmom=cl->TrackMom();
                Float_t deltaeta=cl->DeltaEta();
                Float_t deltaphi=cl->DeltaPhi();
                Float_t pointflag=cl->PointFlag();
//Fill the Histograms
	if(energy>0)m_point_energy->Fill(energy);
	m_point_eta->Fill(eta);
	m_point_phi->Fill(phi);
	m_point_sigeta->Fill(sigmaeta);
	m_point_sigphi->Fill(sigmaphi);
	m_point_flag->Fill(pointflag+1);
	if(trackmom>0){
          m_point_trmom->Fill(trackmom);
	  m_point_deleta->Fill(deltaeta);
	  m_point_delphi->Fill(deltaphi);
        }
               }
              }
            }
          }
}
//-------------------------------------------------------------------------
Int_t StEpcMaker::fillStEvent()
{
  if(!m_DataSet)return kStOK;
   StEmcCollection *emc;
  if(!mTheEmcCollection){
    cout<<"Epc:: Emc Collection does not exist, Create one***"<<endl;
  //Create StEmcHitCollection
   emc = new StEmcCollection();
   cout<<" Epc:: EmcCollection created***"<<endl;
     }
     else{
      cout<<" Epc:: EmcCollection exist***"<<endl;
      emc=mTheEmcCollection;
     }
   cout<<" Epc:: Main fillevent***"<<endl;

  //create StEmcCollection
//  StEmcCollection* emc = new StEmcCollection();

  //Add it to Dataset as ObjectSet
     AddData(new St_ObjectSet("EmcCollection", emc));

	  St_DataSetIter itr(m_DataSet);
	  StPointCollection *cluster = 0;
	  StPointCollection dummy;
	  TString tit = dummy.GetTitle();
	  cluster= (StPointCollection*)itr();
	  if(cluster != 0){
	    if(cluster->GetTitle() == tit){
	      Int_t nR = cluster->NPointsReal();

	      if(nR>0){
		TIter next(cluster->PointsReal());
		StEmcPoint *cl;
		for(Int_t i=0; i<nR; i++){
		  cl = (StEmcPoint*)next();
		  emc->addBarrelPoint(cl);
		} //for i=0
	      }// for nR
	    }//tit check
	  } //cluster!= 0 check
  // add EmcSollection to StEvent
//  mEvent->setEmcCollection(emc);
	 
       return kStOK;
	  }
  //-------------------------------------------------------

Int_t StEpcMaker::Finish() {
  return StMaker::Finish();
}
//
bool StEpcMaker::accept(StTrack* track)
{
    //
    //  This is a kind of very simple track filter.
    //  We only check for positive flags.
    //  Note that this method works for global and
    //  primary tracks since we deal with the base
    //  class only (StTrack).
    //
    return track && track->flag() >= 0;
}
   
