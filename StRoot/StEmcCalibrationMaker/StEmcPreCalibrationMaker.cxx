//*-- Author : Alexandre Suaide 
// 
// $Id: StEmcPreCalibrationMaker.cxx,v 1.1 2001/09/24 13:30:49 suaide Exp $
// $Log: StEmcPreCalibrationMaker.cxx,v $
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
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StEmcPreCalibrationMaker::Finish()
{
  cout <<"Finishing EmcPreCalibration maker ++++++++++++++++++++++++\n";
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StEmcPreCalibrationMaker::Make()
{  
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  if(currevent)
  {
   delete currevent;
  }
  
  currevent=new StEvent();
	
  mTheEmcData   = GetDataSet("StDAQReader");
 	if(!mTheEmcData) { return kStWarn;}
 	      
  StEmcCollection* emc = GetEmcCollectionFromDaq(mTheEmcData);
  if(!emc) return kStWarn;
  if(currevent->emcCollection()) delete currevent->emcCollection();
  currevent->setEmcCollection(emc);
	
  StDAQReader* mTheDataReader = (StDAQReader*)(mTheEmcData->GetObject());
  if(!mTheDataReader->L3Present()) return kStWarn;
  StL3Reader *L3=mTheDataReader->getL3Reader();
  if(!L3) return kStWarn;
  
  GlobalTrackReader *L3Tracks=L3->getGlobalTrackReader();
  if(!L3Tracks) return kStWarn;
  
  Int_t ntracks=L3Tracks->getNumberOfTracks();
  globalTrack *tracks=L3Tracks->getTrackList();
  

  StL3Trigger* l3t = new StL3Trigger();
  StSPtrVecTrackNode& nodes = l3t->trackNodes() ;

  Float_t cte=3.1415926/180.; // check if L3 angles are dregee or radians....
  
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

    gTrack->setLength(tracks[i].length);    
    gTrack->setGeometry(helixModel);
    
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










