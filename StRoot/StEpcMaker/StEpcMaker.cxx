//
// $Id: StEpcMaker.cxx,v 1.14 2001/11/01 00:15:23 suaide Exp $
// $Log: StEpcMaker.cxx,v $
// Revision 1.14  2001/11/01 00:15:23  suaide
// Clean up and small modification to stop crashing at Finish()
//
// Revision 1.13  2001/10/24 13:55:05  suaide
// small bugs fixed
//
// Revision 1.12  2001/10/15 01:41:41  pavlinov
// Added Clear method
//
// Revision 1.11  2001/10/03 17:27:37  pavlinov
// clean up for production
//
// Revision 1.10  2001/07/25 15:29:18  subhasis
// check if clusters exist in bemc , bsmde and bsmdp
//
// Revision 1.9  2001/04/25 17:26:13  perev
// HPcorrs
//
// Revision 1.8  2001/04/24 23:06:08  subhasis
// clusters attached to Points, QA hists are made for all category separately
//
// Revision 1.6  2000/08/29 20:40:06  subhasis
//  Modified to accept input from StEvent and writing output to StEvent for Emc
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
#include "StEmcUtil/StEmcGeom.h"
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
extern "C" {void type_of_call assndx ( Int_t &, Float_t *, Int_t &, Int_t &, Int_t &,Int_t *, Float_t &,Int_t *,Int_t &); }


ClassImp(StEpcMaker)

const TString detname[] = {"Bemc", "Bsmde", "Bsmdp"};
//_____________________________________________________________________________
StEpcMaker::StEpcMaker(const char *name):StMaker(name)
{
  //  drawinit=kFALSE;
}
//_____________________________________________________________________________
StEpcMaker::~StEpcMaker()
{
}
//________________________________________________________________________
Int_t StEpcMaker::Init()
{
//Making QA histgrams
// for points


  const TString catname[] = {"Cat1", "Cat2", "cat3", "cat4"};
  for (Int_t i=0; i<4; i++)
  {
      TString name_e = catname[i] + "_Point_Energy";
      TString tit_e = catname[i] + " Point Energy";
      m_point_energy[i]= new TH1F(name_e,tit_e,100,0.,10.);


      TString name_eta = catname[i] + "_Point_Eta";
      TString tit_eta = catname[i] + " Point Eta";
      m_point_eta[i]= new TH1F(name_eta,tit_eta,100,-1.,1.);

      TString name_phi = catname[i] + "_Point_Phi";
      TString tit_phi = catname[i] + " Point Phi";
      m_point_phi[i]= new TH1F(name_phi,tit_phi,100,-3.14,3.14);
   
      TString name_sigeta = catname[i] + "_Sigma_Eta";
      TString tit_sigeta = catname[i] + " Sigma Eta";
      m_point_sigeta[i]= new TH1F(name_sigeta,tit_sigeta,100,0.,.2);
  
      TString name_sigphi = catname[i] + "_Sigma_Phi";
      TString tit_sigphi = catname[i] + " Sigma Phi";
      m_point_sigphi[i]= new TH1F(name_sigphi,tit_sigphi,100,0.,.2);

      TString name_deleta = catname[i] + "_Delta_Eta";
      TString tit_deleta = catname[i] + " Delta Eta";
      m_point_deleta[i]= new TH1F(name_deleta,tit_deleta,100,-.5,.5);

      TString name_delphi = catname[i] + "_Delta_Phi";
      TString tit_delphi = catname[i] + " Delta Phi";
      m_point_delphi[i]= new TH1F(name_delphi,tit_delphi,100,-.5,.5);

      TString name_points = catname[i] + "_Points_Multiplicity";
      TString tit_points = catname[i] + " Points Multiplicity";
      m_emc_points[i]= new TH1F(name_points,tit_points,200,0.,2000.);

      TString name_mom = catname[i] + "_Track_Momenta";
      TString tit_mom = catname[i] + " Track Momenta ";
      m_point_trmom[i]= new TH1F(name_mom,tit_mom,100,0.,10.);
  }

  m_point_flag= new TH1F(" Point Flag "," Point Flag ",5,0.5,5.5);
  return StMaker::Init();
}  
//_________________________________________________________________________
Int_t StEpcMaker::Make()
{
  cout<<" StEpcMaker Make() **"<<endl;
  // ptr initialization
  mTheEmcCollection=0;                                                            

  if (!m_DataSet->GetList()) //if DataSet is empty, create object and fill it
  {   
    //Getting tracks from StEvent
    cout << "about to Obtain Stevent" << endl;
    mEvent = (StEvent *) GetInputDS("StEvent");
 
    if (!mEvent) 
    {
      cout << "No StEvent! Can not continue. " << endl;
      return kStOK; // If no event, we're done
    }
    cout<<"StEpcMaker** , StEvent found**"<<endl; 
    //First Find if EmcCollection exists

    if(mEvent)
    {
      cout<<"Epc::StEvent found **"<<endl;
      mTheEmcCollection = mEvent->emcCollection();
    }
    if(!mTheEmcCollection)
    {
      cout<<" EPC:: No EmcCollection, Cannot continue**"<<endl;
      return kStOK;
    }

    StDetectorId EmcId;
    StEmcDetector* EmcDet;
    StEmcClusterCollection* cluscoll=NULL;
    StEmcClusterCollection* Bemccluster=NULL;
    StEmcClusterCollection* Bprscluster=NULL;
    StEmcClusterCollection* Bsmdecluster=NULL;
    StEmcClusterCollection* Bsmdpcluster=NULL;

    if(mTheEmcCollection)
    {
      cout<<" StEvent EmcCollection exists**"<<endl;

      for(Int_t idet=0;idet<4;idet++)
      {
        EmcId = static_cast<StDetectorId>(idet+kBarrelEmcTowerId); 
        EmcDet =mTheEmcCollection->detector(EmcId);
        cluscoll=NULL;
        if(EmcDet)
        {
          cluscoll = EmcDet->cluster();
          UInt_t ncl=0;
          if(cluscoll) ncl = cluscoll->numberOfClusters();
          
          // added to check if clusters exist for all detectors (not PRS now).
          if(idet !=1 && ncl==0)
          {
            cout<<" NO Clusters for detector " << idet <<endl;
            return kStWarn;
          }

          if(cluscoll)
          {
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

    for (size_t nodeIndex=0; nodeIndex<theTrackNodes.size(); nodeIndex++) 
    {
      size_t numberOfTracksInNode =  theTrackNodes[nodeIndex]->entries(global);
      for (size_t trackIndex=0; trackIndex<numberOfTracksInNode; trackIndex++) 
      {
        track =theTrackNodes[nodeIndex]->track(global,trackIndex);
        if (track) allGlobals++;
        if (accept(track))
        {
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
    {
      return kStErr;
    } 
    else cout<<" findEmcPoint called"<<endl;

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
  Int_t Point_Mult[4];
  for(UInt_t i=0;i<4;i++) Point_Mult[i]=0;

  St_DataSetIter itr(m_DataSet);
  StPointCollection *cluster = 0;
  StPointCollection dummy;
  TString tit = dummy.GetTitle(); 
  cluster = (StPointCollection*)itr();
  if(cluster !=0)
  {
    if(cluster->GetTitle() == tit)
    {
      Int_t n = cluster->NPoints();
	    cout<<"Make hist**n***"<<n<<endl;
//	    m_emc_points->Fill(Float_t(n));
      if(n>0)
      {
        TIter next(cluster->Points());
        StPi0Candidate *cl;
        for(Int_t i=0;i<n;i++)
        {
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
          Int_t ncat=Int_t(pointflag);
          if(ncat>3)ncat=3;
          Point_Mult[Int_t(pointflag)]++;

          //Fill the Histograms
		
	        if(energy>0)m_point_energy[ncat]->Fill(energy);
	        m_point_eta[ncat]->Fill(eta);
	        m_point_phi[ncat]->Fill(phi);
	        m_point_sigeta[ncat]->Fill(sigmaeta);
	        m_point_sigphi[ncat]->Fill(sigmaphi);
	        m_point_flag->Fill(pointflag+1);
	        if(trackmom>0)
          {
            m_point_trmom[ncat]->Fill(trackmom);
	          m_point_deleta[ncat]->Fill(deltaeta);
	          m_point_delphi[ncat]->Fill(deltaphi);
          }
        }
      }
    }
  }
  for(UInt_t i=0;i<4;i++) m_emc_points[i]->Fill(Float_t(Point_Mult[i]));
}
//-------------------------------------------------------------------------
Int_t StEpcMaker::fillStEvent()
{
  if(!m_DataSet)return kStOK;
  StEmcCollection *emc;
  if(!mTheEmcCollection)
  {
    cout<<"Epc:: Emc Collection does not exist, Create one***"<<endl;
    //Create StEmcCollection
    emc = new StEmcCollection();
    AddData(new St_ObjectSet("EmcCollection", emc));
    cout<<" Epc:: EmcCollection created***"<<endl;
  }
  else
  {
    cout<<" Epc:: EmcCollection exist***"<<endl;
    emc=mTheEmcCollection;
  }
   cout<<" Epc:: Main fillevent***"<<endl;

  St_DataSetIter itr(m_DataSet);
  StPointCollection *cluster = 0;
  StPointCollection dummy;
  TString tit = dummy.GetTitle();
  cluster= (StPointCollection*)itr();
  if(cluster != 0)
  {
    if(cluster->GetTitle() == tit)
    {
      Int_t nR = cluster->NPointsReal();
      if(nR>0)
      {
		    TIter next(cluster->PointsReal());
		    StEmcPoint *cl;
		    for(Int_t i=0; i<nR; i++)
        {
		      cl = (StEmcPoint*)next();
		      emc->addBarrelPoint(cl);
		    } //for i=0
	    }// for nR
	  }//tit check
	} //cluster!= 0 check
  return kStOK;
}
//-------------------------------------------------------
Int_t StEpcMaker::Finish() 
{
}
//-------------------------------------------------------
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
//-------------------------------------------------------
void StEpcMaker::Clear(Option_t *option)
{// 14-oct-2001
  if(option){};
}
