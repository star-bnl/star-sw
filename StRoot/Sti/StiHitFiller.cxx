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
    cout <<"StiHitFiller::fillHits()"<<endl;
    for (det_id_vector::const_iterator it=mvec.begin(); it!=mvec.end(); ++it) {
	if ( *it == kTpcId) fillTpcHits(store, factory);
	if ( *it == kSvtId) fillSvtHits(store, factory);
    }
    fillPrimaryVertices(store, factory);
    cout <<"StiHitFiller::fillHits()\tDone Filling Hits"<<endl;
    return;
}

void StiHitFiller::fillTpcHits(StiHitContainer* store, StiHitFactory* factory)
{
    cout <<"StiHitFiller::fillTpcHits()"<<endl;
    double nhit=0;
    double nprint = 1000.;
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
		    ++nhit;
		    if (fmod(nhit, nprint)==0.) {
			cout <<"Filling TpcHit:\t"<<nhit<<endl;
		    }
		}
	    }
	}	    
    }
    
    return;
}

void StiHitFiller::fillSvtHits(StiHitContainer* store, StiHitFactory* factory)
{
    cout <<"StiHitFiller::fillSvtHits()"<<endl;
    double nhit=0;
    double nprint = 1000.;
    StSvtHitCollection* svthits = mevent->svtHitCollection();
    //loop on barrels
    for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) {
	StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
	//Loop on ladders
	for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) {
	    StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	    //Loop on wafers
	    for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
		//cout <<"Barrel "<<barrel<<"\tladder "<<ladder<<"\twafer "<<wafer<<endl;
		StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
		const StSPtrVecSvtHit& hits = waferhits->hits();  //Finally!
		for (vector<StSvtHit*>::const_iterator it=hits.begin(); it!=hits.end(); ++it) {
		    StSvtHit* hit = dynamic_cast<StSvtHit*>(*it);
		    if (hit) {
			if (hit->flag()<4) {
			    //Now we've got the hit, fillerup!
			    StiHit* stihit = factory->getObject();
			    stihit->reset();
			    
			    mtranslator->operator()(hit, stihit);
			    
			    //Now Fill the Hit Container!
			    store->push_back( stihit );
			    ++nhit;
			    if (fmod(nhit, nprint)==0.) {
				cout <<"Filling TpcHit:\t"<<nhit<<endl;
			    }
			}
		    }
		}
	    }
	}
    }
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

