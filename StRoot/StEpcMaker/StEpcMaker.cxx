//
// $Id: StEpcMaker.cxx,v 1.4 2000/07/03 02:07:45 perev Exp $
// $Log: StEpcMaker.cxx,v $
// Revision 1.4  2000/07/03 02:07:45  perev
// StEvent: vector<TObject*>
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
 
#include "St_ObjectSet.h"
#include "StEmcCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StEmcRawHit.h"
#include "StEmcClusterCollection.h"
#include "StEmcCluster.h"
#include "StEmcPoint.h"
#include "StEnumerations.h"                                                      
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

// declaring cernlib routine (mathlib, H301) assndx to be used for matching.
#include "StarCallf77.h"
#define    assndx  F77_NAME(assndx,ASSNDX)
extern "C" {void type_of_call assndx ( Int_t &, Float_t *, Int_t &, Int_t &, Int_t &,Int_t *, Float_t &,Int_t *,Int_t &); }


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

  return StMaker::Init();
}  
//_________________________________________________________________________
Int_t StEpcMaker::Make(){
  if (!m_DataSet->GetList()){   //if DataSet is empty, create object and fill it
  //
    StEmcHitCollection *hit = 0;    
    StEmcHitCollection *hits = 0;    
    StEmcHitCollection *hit_e = 0;    
    StEmcHitCollection *hit_p = 0;    
    StBemcPreClusterCollection *hit0 = 0;
    StBsmdePreClusterCollection *hit1 = 0;
    StBsmdpPreClusterCollection *hit2 = 0;
    StEmcHitCollection dummy; TString tit = dummy.GetTitle();
    // get pointer for StEmcHitCollection
    St_DataSetIter itr(GetDataSet("emc_hits"));
   Int_t nhitsw =0;
   Int_t nhit =0;
   Int_t cluster_tot=0;

    while ( (hit = (StEmcHitCollection *)itr()) ) {
      if(hit->GetTitle() == tit){
        TString name = hit->GetName();
        nhitsw = hit->NHit();
        nhit +=nhitsw;
        if(nhitsw > 0){
          if(!strcmp(name.Data(),"bemc")){
          // cout <<" Name "<< name.Data() <<"  Hits " << nhitsw << endl;
          hits = (StEmcHitCollection *)hit;
          }
          else if(!strcmp(name.Data(),"bsmde")){ 
          // cout <<" Name "<< name.Data() <<"  Hits " << nhitsw << endl;
          hit_e = (StEmcHitCollection *)hit;
          }
          else if(!strcmp(name.Data(),"bsmdp")){
          // cout <<" Name "<< name.Data() <<"  Hits " << nhitsw << endl;
          hit_p = (StEmcHitCollection *)hit;
          } 
        }   // nhitsw if end
    else{
          cout <<" nhitsw<=0 Name "<< hit->GetName()<<"  Hits " << nhitsw << endl;
//	return kStWarn;
        }
      } // tit if end
   } // while loop end for StEmcHitCollection

   cout<<" Total number of hits **"<<nhit<<endl;

//If there is no Emc hit, return
  if(nhit<=0){
    cout<<" StEpcMaker:: No Emc Hits Found ***, Returning"<<endl;
    return kStWarn;
  }

//  If there is EMC hit, then get the clusters

  if(nhit > 0){
    St_DataSetIter itr(GetDataSet("preecl"));
    StBemcPreClusterCollection dummy; TString tit = dummy.GetTitle();
    Char_t *cdet;
    cdet="bemc";
   hit0 = (StBemcPreClusterCollection *)itr(cdet);
  if(hit0 != 0){
      if(hit0->GetTitle() == tit){
        Int_t nhit0 = hit0->Nclusters();
        cluster_tot +=nhit0;
         // cout <<"  Hit0  " << nhit0 << endl;
      }
   }

    cdet="bsmde";
   hit1 = (StBsmdePreClusterCollection *)itr(cdet);
  if(hit1 != 0){
      if(hit1->GetTitle() == tit){
        Int_t nhit1 = hit0->Nclusters();
        cluster_tot +=nhit1;
         // cout <<"  Hit1  " << nhit1 << endl;
      }
   }

    cdet="bsmdp";
   hit2 = (StBsmdpPreClusterCollection *)itr(cdet);
  if(hit2 != 0){
      if(hit2->GetTitle() == tit){
        Int_t nhit2 = hit2->Nclusters();
        cluster_tot +=nhit2;
         // cout <<"  Hit2  " << nhit2 << endl;
      }
   }
}

   cout<<" Total number of clusters **"<<cluster_tot<<endl;

//If there is no Emc cluster, return
  if(cluster_tot<=0){
    cout<<" StEpcMaker::  No Emc Cluster Found ***, Returning"<<endl;
    return kStWarn;
  }


  // Getting tracks from globals
   
  St_DataSet *match = GetDataSet("match");
  St_DataSetIter matchI(match);
 
  St_dst_track     *globtrk  = (St_dst_track *) matchI("globtrk");
  long NGlbTrk = globtrk->GetNRows(); 

//******Creating StPointCollection and calling findPoints
	StPointCollection *point = new StPointCollection("point");
	m_DataSet->Add(point);

if(point->findPoints(hit0,hit1,hit2,hits,hit_e,hit_p,globtrk)!=kStOK)
 { return kStErr;}
else
 {
       cout<<" FindPoint called"<<endl;
 }
  MakeHistograms(); // Fill QA histgrams
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

  //create StEmcCollection
  StEmcCollection* emc = new StEmcCollection();

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
	      }// for nR>0 check
	    }//tit check
	  } //cluster!= 0 check

	  return kStOK;
	  }
  //-------------------------------------------------------

Int_t StEpcMaker::Finish() {
  return StMaker::Finish();
} 

