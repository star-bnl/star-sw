//StiHitFiller.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>
#include <cmath>

// StEvent
#include "StEventTypes.h"

//StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

///Sti
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiObjectFactory.h"
#include "StiGeometryTransform.h"

#include "StiHitFiller.h"


//StiHitFiller::StiHitFiller() : mtranslator(0), mtpctransformer( new StTpcCoordinateTransform(gStTpcDb) )
StiHitFiller::StiHitFiller() : mtranslator(StiGeometryTransform::instance())
{
    cout <<"\nStiHitFiller::StiHitFiller()\n"<<endl;

}

StiHitFiller::~StiHitFiller()
{
    cout <<"\nStiHitFiller::~StiHitFiller()\n"<<endl;
}

void StiHitFiller::addDetector(StDetectorId det)
{
    mvec.push_back(det);
    return;
}

void StiHitFiller::fillHits(StiHitContainer* store, StiHitFactory* factory)
{
    for (det_id_vector::const_iterator it=mvec.begin(); it!=mvec.end(); ++it) {
	if ( *it == kTpcId) fillTpcHits(store, factory);
	if ( *it == kSvtId) fillSvtHits(store, factory);
    }
    fillPrimaryVertices(store, factory);
    return;
}

void StiHitFiller::fillTpcHits(StiHitContainer* store, StiHitFactory* factory)
{
    const StTpcHitCollection* tpcHits = mevent->tpcHitCollection();    
    //Loop over sectors
    for (int sector=1; sector<=24; sector++) {
	const StTpcSectorHitCollection* secHits = tpcHits->sector(sector-1);
	//Loop over padrows
	for (int prow=1; prow<=45; prow++) {
	    const StTpcPadrowHitCollection* padrowHits = secHits->padrow(prow-1);
	    const StSPtrVecTpcHit& hitvec = padrowHits->hits();
	    //Loop over hits
	    for (StSPtrVecTpcHitIterator iter = hitvec.begin(); iter != hitvec.end(); iter++) {
		StTpcHit* hit = dynamic_cast<StTpcHit*>(*iter);
		if (hit) {
		    
		    //Now we have the hit
		    StiHit* stihit = factory->getObject();
		    stihit->reset();
		    
		    mtranslator->operator()(hit, stihit);

		    //Now Fill the Hit Container!
		    store->push_back( stihit );
		}
	    }
	}	    
    }
    
    return;
}

void StiHitFiller::fillSvtHits(StiHitContainer* store, StiHitFactory* factory)
{
    return;
}

void StiHitFiller::fillPrimaryVertices(StiHitContainer* store, StiHitFactory* factory)
{
    StPrimaryVertex* primVtx = 0;
    for(unsigned int i=0; i<mevent->numberOfPrimaryVertices(); i++) {
	primVtx = mevent->primaryVertex(i);
	
	StiHit* stihit = factory->getObject();
	stihit->reset();
	
	mtranslator->operator() ( primVtx, stihit);
	
	store->addVertex(stihit);
	//cout <<primVtx->position()<<endl;
    }
    return;
}

ostream& operator<<(ostream& os, const StiHitFiller& h)
{
    for (StiHitFiller::det_id_vector::const_iterator it=h.mvec.begin();
	 it!=h.mvec.end(); ++it) {
	os <<static_cast<int>( (*it) )<<" ";
    }
    return os;
}

