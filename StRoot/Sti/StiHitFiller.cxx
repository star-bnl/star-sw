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

double gRefAnleForSector(unsigned int sector);

//StiHitFiller::StiHitFiller() : mtranslator(StiGeometryTransform::instance())
StiHitFiller::StiHitFiller() : mtranslator(0), mtpctransformer( new StTpcCoordinateTransform(gStTpcDb) )
{
    cout <<"\nStiHitFiller::StiHitFiller()\n"<<endl;

    //Temp, build map of padrow radius
    cout <<"Generating Padrow Radius Map"<<endl;
    for (unsigned int padrow=1; padrow<=45; ++padrow) {
	double center = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
	mpadrowradiusmap.insert( padrow_radius_map_ValType( padrow, center ) );	
    }
    cout <<"\nPadrow\tRadius"<<endl;
    for (padrow_radius_map::const_iterator it=mpadrowradiusmap.begin(); it!=mpadrowradiusmap.end(); ++it) {
	cout <<(*it).first<<"\t"<<(*it).second<<endl;
    }
}

StiHitFiller::~StiHitFiller()
{
    cout <<"\nStiHitFiller::~StiHitFiller()\n"<<endl;
    delete mtpctransformer;
    mtpctransformer=0;
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
		    
		    this->operator()(hit, stihit);
		    //mtranslator->operator()(hit, stihit);

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
	
	this->operator() ( primVtx, stihit);
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

void StiHitFiller::operator() (const StPrimaryVertex* vtx, StiHit* stihit)
{
    //A primary vertex doesn't come from a detector, so it doesn't have a well defined refAngle and centerRadius
    //We'll define these two from global position in cylindrical coordinates
    //refAngle = arctan(global_y / global_x)
    //centerRadius = sqrt (global_x^2 + global_y^2)
    //We'll then say that Sti_x = centerRadius and Sti_y = 0, with Sti_z begin global z, as usual

    const StThreeVectorF& position = vtx->position();
    double pos = sqrt(position.x()*position.x() + position.y()*position.y() );
    double refangle = atan2( position.y(), position.x() );

    if (refangle<0.) refangle+=2.*M_PI;
    
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );
    stihit->setX( pos );
    stihit->setY( 0. );
    stihit->setZ( position.z() );
    
    return;
}

void StiHitFiller::operator() (const StTpcHit* tpchit, StiHit* stihit)
{
    //Change if we change numbering scheme
    double refangle = gRefAnleForSector( tpchit->sector() );
    double pos = mpadrowradiusmap[ tpchit->padrow() ];
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );

    //Make Tpc hits
    StGlobalCoordinate gHit( tpchit->position() );
    StTpcLocalSectorCoordinate lsHit;

    //Transform 
    mtpctransformer->operator()(gHit, lsHit);

    //Careful, we have to swap z for all hits, and x and y for hits with global z>0

    //Keep z in global coordinates
    stihit->setZ( tpchit->position().z() );

    //Take x -> -x, then swap x for y
    if (tpchit->position().z() >0) {
	stihit->setX( lsHit.position().y() );
	stihit->setY( -1.*lsHit.position().x() );
    }

    //Swap x for y, 
    else {
	stihit->setX( lsHit.position().y() );
	stihit->setY( lsHit.position().x() );
    }

    /*
      cout <<"TpcHit: "<<tpchit->sector()<<" "<<tpchit->padrow()<<" "<<tpchit->position();
      cout <<" GlobHit: "<<gHit.position();
      cout <<" LSHit: "<<lsHit.position()<<" From Sector: "<<lsHit.fromSector()<<" ";
      cout <<" StiHit: "<<stihit->x()<<" "<<stihit->y()<<" "<<stihit->z()<<endl;
    */
    return;
}

double gRefAnleForSector(unsigned int sector)
{
    unsigned int numSectors = 24;
    double tolerance = .00001;
    double beta = (sector > 12) ?(numSectors-sector)*2.*M_PI/(static_cast<double>(numSectors)/2.) :
	sector*2.*M_PI/(static_cast<double>(numSectors)/2.);
    return ( fabs(2.*M_PI - beta) < tolerance ) ? fabs(2.*M_PI-beta) : beta;
}
