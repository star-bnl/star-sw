//StiHitFiller.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

// StEvent
#include "StEventTypes.h"

// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

//Sti
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiHitFactory.h"

#include "StiHitFiller.h"

StiHitFiller::StiHitFiller()
{
    cout <<"\nStiHitFiller::StiHitFiller()\n"<<endl;
    //mtransformer = new StTpcCoordinateTransform(gStTpcDb);
}

StiHitFiller::~StiHitFiller()
{
    cout <<"\nStiHitFiller::~StiHitFiller()\n"<<endl;
    //delete mtransformer;
    //mtransformer = 0;
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
