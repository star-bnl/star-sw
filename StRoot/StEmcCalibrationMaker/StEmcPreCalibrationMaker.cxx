//*-- Author : Alexandre Suaide 
// 
// $Id: StEmcPreCalibrationMaker.cxx,v 1.5 2001/11/07 17:54:10 suaide Exp $
// $Log: StEmcPreCalibrationMaker.cxx,v $
// Revision 1.5  2001/11/07 17:54:10  suaide
// some modifications for real data
//
// Revision 1.3  2001/10/26 21:00:33  suaide
// Many modifications to optimize for real data
//
// Revision 1.2  2001/10/17 13:51:31  suaide
// new modifications to work with real data
//
// Revision 1.1  2001/09/24 13:30:49  suaide
// Added Effective pedestal calculation and Pre Calibration Maker to
// generate EMC and L3 StEvent objects from Daq file
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//
#include "StEmcPreCalibrationMaker.h"
#include "StChain.h"
#include <iostream.h>
#include <math.h>
#include "St_DataSetIter.h"
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StGlobals.hh"
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

#include "StEmcUtil/StEmcDaqUtil.h"
ClassImp(StEmcPreCalibrationMaker)

//_____________________________________________________________________________
StEmcPreCalibrationMaker::StEmcPreCalibrationMaker(const char *name,int daq)
                         :StMaker(name),mDaq(daq)
{
}
//_____________________________________________________________________________
StEmcPreCalibrationMaker::~StEmcPreCalibrationMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcPreCalibrationMaker::Init()
{
  cout <<"Starting EmcPreCalibration maker ++++++++++++++++++++++++\n";
  etaDistr=new TH2F("etaDistr","nHits x eta",300,-2,2,200,0,200);
  pDistr=new TH2F("pDistr","nHits x momentum",1000,0,5,200,0,200);
  pDistr1=new TH2F("pDistr1","nHighPt x eta",300,-2,2,1000,0,5);
  return StMaker::Init();
}
//_____________________________________________________________________________
/*void StEmcPreCalibrationMaker::Clear(Option_t *option)
{
  cout <<"Cleaning EmcPreCalibration maker ++++++++++++++++++++++++\n";
  //return StMaker::Clear();
}*/
//_____________________________________________________________________________
Int_t StEmcPreCalibrationMaker::Finish()
{
  cout <<"Finishing EmcPreCalibration maker ++++++++++++++++++++++++\n";
}
//_____________________________________________________________________________
Int_t StEmcPreCalibrationMaker::Make()
{  
  //StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  //if(currevent)
  //{
  // delete currevent;
  //}
  currevent=NULL;
  emccol=NULL;
  currevent=new StEvent();
	
  mTheEmcData   = GetDataSet("StDAQReader");
 	if(!mTheEmcData) { return kStWarn;}
  
  StDAQReader* TheDataReader=(StDAQReader*)(mTheEmcData->GetObject());
  unsigned int evtime= TheDataReader->getUnixTime();
  cout <<"evtime = "<<evtime<<"  GetDate = "<<GetDate()<<"  GetTime = "<<GetTime()<<endl;
 	      
  emccol = GetEmcCollectionFromDaq(mTheEmcData);
  if(!emccol) 
  {
    cout <<"StEmcPreCalibrationMaker:: No EMC\n";
    return kStWarn;
  }
  currevent->setEmcCollection(emccol);
  
  
  StDAQReader* mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
  //if(!mTheDataReader->L3Present()) 
  //{
  //  cout <<"StEmcPreCalibrationMaker:: No L3\n";
  //  return kStWarn;
  //}
  StL3Reader *L3=mTheDataReader->getL3Reader();
  if(!L3) return kStWarn;
  
  GlobalTrackReader *L3Tracks=L3->getGlobalTrackReader();
  if(!L3Tracks) return kStWarn;
  
  Int_t ntracks=L3Tracks->getNumberOfTracks();
  globalTrack *tracks=L3Tracks->getTrackList();
  cout <<"StEmcPreCalibrationMaker:: Number of global tracks = "<<ntracks<<endl;

  StL3Trigger* l3t = new StL3Trigger();
  StSPtrVecTrackNode& nodes = l3t->trackNodes() ;

  Float_t cte=1.; // check if L3 angles are dregee or radians....
  
  for(Int_t i=0;i<ntracks;i++)
  {
    StGlobalTrack *gTrack = new StGlobalTrack();
    
    Float_t psi = tracks[i].psi*cte;
    Float_t tanl=tracks[i].tanl;
    Short_t charge = tracks[i].q;
    Float_t curvature=0;
    Float_t pt=tracks[i].pt;
    Float_t r0=tracks[i].r0;
    Float_t phi0=tracks[i].phi0*cte;
    Float_t z0=tracks[i].z0;
    StThreeVectorF momentum( pt * cos(psi), pt * sin(psi), pt * tanl ) ;
    StThreeVectorF origin( r0*cos(phi0),r0*sin(phi0),z0) ;
    StHelixModel* helixModel = new StHelixModel(charge,psi,curvature,tanl,origin,momentum,0) ;
    //cout <<"Track "<<i<<"  p = "<<momentum.mag()<<"  pt = "<<pt<<"  q = "<<charge<<"  psi = "<<psi<<"  phi0 = "<<phi0<<"  tanl = "<<tanl<<endl;

    gTrack->setLength(tracks[i].length);    
    gTrack->setGeometry(helixModel);
    
    /*StTrackDetectorInfo* detecInfo = new StTrackDetectorInfo() ;
    detecInfo->setNumberOfPoints(tracks[i].nHits) ;
    gTrack->setDetectorInfo(detecInfo) ;*/
    
    Float_t a[2],b[15];
    StTrackFitTraits fit(0,(Int_t)tracks[i].nHits,a,b);
    gTrack->setFitTraits(fit);
        
    pDistr->Fill(momentum.mag(),(Int_t)tracks[i].nHits);
    Float_t R=230;
    Float_t dr=R-r0;
    StThreeVectorF tmp(dr*cos(psi),dr*sin(psi),dr*tanl);
    StThreeVectorF tmp1=tmp+origin;
    Float_t eta=tmp1.pseudoRapidity();      
    pDistr1->Fill(eta,momentum.mag());
    if(momentum.mag()>1.2) 
    {
      etaDistr->Fill(eta,(Int_t)tracks[i].nHits);
      /*if(eta>=0 && eta<=1) cout <<" track "<<i<<"  p = "<<momentum.mag()
                                <<"  nHits = "<<(Int_t)tracks[i].nHits
                                <<"  z0 = "<<z0
                                <<"  r0 = "<<r0
                                <<"  tanl = "<<tanl
                                <<"  dr = "<<dr
                                <<"  etaEMc = "<<eta<<endl;*/
    }
    
    StTrackNode* trackNode = new StTrackNode() ;
    trackNode->addTrack(gTrack) ;
    nodes.push_back(trackNode) ; 
  }
  vertex l3v=L3Tracks->getVertex();
  StThreeVectorF vv(l3v.x,l3v.y,l3v.z);
  StPrimaryVertex *v=new StPrimaryVertex();
  v->setPosition(vv);
  
  l3t->addPrimaryVertex(v);

  currevent->setL3Trigger(l3t);
	    
  AddData(currevent);
  return kStOK;
}










