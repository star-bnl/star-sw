// $Id: StiBTofHitLoader.cxx,v 1.2 2012/06/11 15:32:14 fisyak Exp $
// $Log: StiBTofHitLoader.cxx,v $
// Revision 1.2  2012/06/11 15:32:14  fisyak
// Fix tray numbers
//
// Revision 1.1.1.1  2012/04/10 13:31:39  fisyak
// The first version
//
#include <iostream>
#include <stdexcept>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include "StEvent.h"
#include "StEventTypes.h"
#include "StBTofHit.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiHit.h"
#include "StRnDHit.h"
#include "StRnDHitCollection.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "StiBTofDetectorBuilder.h"
#include "Sti/StiTrackContainer.h"
#include "StiBTofHitLoader.h"
static Int_t _debug = 0;
void StiBTofHitLoader::loadHits(StEvent* source,
				Filter<StiTrack> * trackFilter,
				Filter<StiHit> * hitFilter) {
  LOG_INFO << " -I- Started" << endl;
  if (!_detector)
    throw runtime_error("StiBTofHitLoader::loadHits(StEvent*) - FATAL - _detector==0");
  if(!_hitContainer)
    throw runtime_error("StiBTofHitLoader::loadHits(StEvent*) - FATAL - _hitContainer==0");
  
  StBTofCollection *col = source->btofCollection();
  if (!col) {
    LOG_ERROR <<"StiBTofHitLoader::loadHits\tERROR:\tNo StBTofCollection"<<endm;
    return;
  }
  StSPtrVecBTofHit& vec = col->tofHits();
  StiDetector *detector=0;
  Int_t nHit=0;
  for(UInt_t j=0; j<vec.size(); j++)	{
    StBTofHit *aHit = vec[j];
    if(!aHit)   throw runtime_error("StiBTofHitLoader::loadHits(StEvent*) -E- NULL hit in container");
    if (_debug) {
      LOG_INFO <<Form("hit tray: %i module: %i cell: %i\n",aHit->tray(), aHit->module(), aHit->cell()) << endm;
    }
    if (aHit->tray()   <= 0 || aHit->tray()   > StBTofHit::kNTray   ||
	aHit->module() <= 0 || aHit->module() > StBTofHit::kNModule ||
	aHit->cell()   <= 0 || aHit->cell()   > StBTofHit::kNCell) continue;
    Int_t stiTray = aHit->tray();
    if (aHit->tray() > 60) stiTray = 176 - aHit->tray();
    stiTray = (stiTray+59)%60 + 1;
    detector= _detector->getDetector(0,stiTray-1);
    if(!detector)       throw runtime_error("StiBTofHitLoader::loadHits(StEvent*) -E- NULL detector pointer");
    if (_debug) {
      LOG_INFO <<"add hit to detector:\t"<<detector->getName()<<endm;
    }
    if (_debug) {
      Double_t angle    = detector->getPlacement()->getNormalRefAngle();
      Double_t radius   = detector->getPlacement()->getNormalRadius();
      Double_t zcenter  = detector->getPlacement()->getZcenter();
      Double_t halfDepth = detector->getShape()->getHalfDepth();
      Double_t halfWidth = detector->getShape()->getHalfWidth();
      Double_t thick     = detector->getShape()->getThickness();
      LOG_INFO << " detector info " << *detector << endm;
      LOG_INFO << " radius = "<< radius << " angle = " << angle << " zCenter = " << zcenter << endm;
      LOG_INFO << " depth = " << halfDepth << " Width = " << halfWidth << " thickness= " << thick << endm; 
      LOG_INFO << " key 1 : " << detector->getKey(1) <<" key 2 : " << detector->getKey(2) << endm; 
    }
    StiHit *stiHit=_hitFactory->getInstance();
    if(!stiHit) throw runtime_error("StiBTofHitLoader::loadHits(StEvent*) -E- stiHit==0");
    stiHit->reset();
    TGeoHMatrix *rot = (TGeoHMatrix *) StiBTofDetectorBuilder::RotMatrices()->FindObject(Form("BTof_Tray_%i_Module_%i",aHit->tray(),aHit->module())); 
    assert(rot);
    const Float_t *xyzLF = aHit->position().xyz();
    Double_t xyzL[3] = {xyzLF[0], xyzLF[1], xyzLF[2]};
    Double_t xyzG[3];
    rot->LocalToMaster(xyzL,xyzG);
    stiHit->setGlobal(detector,aHit,xyzG[0],xyzG[1],xyzG[2],aHit->charge());
#if 0
    stiHit->set(detector, aHit, aHit->charge(),
		      radius+aHit->position().x(), yoffset+aHit->position().y(), zcenter+aHit->position().z());
#endif    
    _hitContainer->add(stiHit);
    if (_debug) {
      LOG_INFO <<" nHit = "<<nHit
	       <<" Tray = "<<aHit->tray()<<" Module = "<<aHit->module()<<" Cell = "<<aHit->cell()
	       <<" x = "<<aHit->position().x()<<" y = "<<aHit->position().y()<<" z = "<<aHit->position().z()<<endm;
    }
    //done loop over hits
    nHit++;
  }
  LOG_INFO <<"StiBTofHitLoader:loadHits -I- Loaded "<<nHit<<" BTof hits."<<endm;
}

