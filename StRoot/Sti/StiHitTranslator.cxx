//StiHitTranslator.cxx
//M.L. Miller (Yale Software)
//04/01

#include <iostream>

//StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

//StEvent
#include "StEventTypes.h"

//Sti
#include "StiHit.h"
#include "StiHitTranslator.h"

StiHitTranslator::StiHitTranslator()
{
}

StiHitTranslator::~StiHitTranslator()
{
}

void StiHitTranslator::operator() (const StTpcHit* tpchit, StiHit* stihit)
{
    //Change if we change numbering scheme
    stihit->setSector( tpchit->sector() );
    stihit->setPadrow( tpchit->padrow() );
    
    StGlobalCoordinate gHit( tpchit->position() );
    StTpcLocalSectorCoordinate lsHit;
    mtpctransformer->operator()(gHit, lsHit);
    stihit->setX( lsHit.position().x() );
    stihit->setY( lsHit.position().y() );
    stihit->setZ( lsHit.position().z() );
    cout <<"TpcHit: "<<tpchit->sector()<<" "<<tpchit->padrow()<<" "<<tpchit->position();
    cout <<" GlobHit: "<<gHit.position();
    cout <<" LSHit: "<<lsHit.position()<<endl;

    return;
}

void StiHitTranslator::operator() (const StSvtHit* svthit, StiHit* stihit)
{
    return;
}
