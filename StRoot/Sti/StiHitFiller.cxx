//StiHitFiller.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

// StEvent
#include "StEventTypes.h"

//Sti
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiObjectFactory.h"
#include "StiGeometryTransform.h"

#include "StiHitFiller.h"

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

void StiHitFiller::fillHits(StiHitContainer* store, StiHitFactory* factory) const
{
    for (det_id_vector::const_iterator it=mvec.begin(); it!=mvec.end(); ++it) {
	if ( *it == kTpcId) fillTpcHits(store, factory);
	if ( *it == kSvtId) fillSvtHits(store, factory);
    }
    return;
}

void StiHitFiller::fillTpcHits(StiHitContainer* store, StiHitFactory* factory) const
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
		    StiHit* stihit = new StiHit();
		    mtranslator->operator()(hit, stihit);
		    //cout <<"TpcHit: "<<hit->sector()<<" "<<hit->padrow()<<" "<<hit->position()<<"\tStiHit: "<<*stihit<<endl;
		    delete stihit;
		}
	    }
	}	    
    }
    
    return;
}

void StiHitFiller::fillSvtHits(StiHitContainer* store, StiHitFactory* factory) const
{
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
