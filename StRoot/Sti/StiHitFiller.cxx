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
#include "StiDetectorFinder.h"
#include "StiDetector.h"

#include "StiHitFiller.h"

ostream& operator<<(ostream&, const StiDetector&);

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
    mtimer.reset();
    mtimer.start();
    
    double nhit=0;
    double nprint = 10000.;
    const StTpcHitCollection* tpcHits = mevent->tpcHitCollection();    
    //Loop over sectors
    for (int sector=1; sector<=24; sector++) {
	const StTpcSectorHitCollection* secHits = tpcHits->sector(sector-1);
	//Loop over padrows
	for (int prow=1; prow<=45; prow++) {
	    const StTpcPadrowHitCollection* padrowHits = secHits->padrow(prow-1);
	    const StSPtrVecTpcHit& hitvec = padrowHits->hits();

	    //Find the detector for this set of hits:
	    char szBuf[100];
	    int iIttfSector=sector;
	    if (sector>12) {
		iIttfSector = 12 - (sector-12)%12;
	    }	    
	    sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d", static_cast<int>(prow), static_cast<int>(iIttfSector));
	    StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
	    if (!layer) {
		cout <<"StiHitFiller::fillTpcHits(). ERROR:\t Detector for (sector,padrow): (";
		cout <<sector<<","<<prow<<") not found.  Abort"<<endl;
		mtimer.stop();
		return;
	    }
	    
	    //Loop over hits   
	    for (vector<StTpcHit*>::const_iterator iter = hitvec.begin(); iter != hitvec.end(); iter++) {
		//Now we have the hit
		StiHit* stihit = factory->getObject();
		stihit->reset();
		
		mtranslator->operator()( *iter, stihit);
		stihit->setDetector( layer );
		
		//Now Fill the Hit Container!
		store->push_back( stihit );
		++nhit;
		if (fmod(nhit, nprint)==0.) {
		    cout <<"Filling TpcHit:\t"<<nhit<<endl;
		}
	    }
	}	    
    }

    mtimer.stop();
    cout <<"Time to fill TPC Hits: "<<mtimer.elapsedTime()<<endl;
    return;
}

void StiHitFiller::fillSvtHits(StiHitContainer* store, StiHitFactory* factory)
{
    cout <<"StiHitFiller::fillSvtHits()"<<endl;
    mtimer.reset();
    mtimer.start();
    double nhit=0;
    double nprint = 1000.;
    StSvtHitCollection* svthits = mevent->svtHitCollection();
    //loop on barrels
    for (unsigned int barrel=1; barrel<=svthits->numberOfBarrels(); ++barrel) {
	StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel-1);
	//Loop on ladders
	for (unsigned int ladder=1; ladder<=barrelhits->numberOfLadders(); ++ladder) {
	    StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder-1);
	    //Loop on wafers
	    for (unsigned int wafer=1; wafer<=ladderhits->numberOfWafers(); ++wafer) {
		//cout <<"Barrel "<<barrel<<"\tladder "<<ladder<<"\twafer "<<wafer<<endl;
		StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer-1);
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
    cout <<"Time to fill SVT Hits: "<<mtimer.elapsedTime()<<endl;
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

