//StiRootDrawableHitContainer.cxx
//M.L. Miller (Yale Software)
//09/01

//scl
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

//StiGui
#include "StiRootDisplayManager.h"
#include "StiRootDrawableHitContainer.h"

StiRootDrawableHitContainer::StiRootDrawableHitContainer()
{
    cout <<"StiRootDrawableHitContainer::StiRootDrawableHitContainer()"<<endl;
    setMarkerSize( 0.3 );
    setColor( 4 );
    setMarkerStyle( 8 );
		setName("AStiRootDrawableHitContainer"); //must come first in list of drawable!!!
    StiRootDisplayManager::instance()->addDrawable(this);
}

StiRootDrawableHitContainer::~StiRootDrawableHitContainer()
{
    cout <<"StiRootDrawableHitContainer::~StiRootDrawableHitContainer()"<<endl;
}

void StiRootDrawableHitContainer::clear()
{
    StiRootDrawableHits::clear();
    StiHitContainer::clear();
}

void StiRootDrawableHitContainer::update()
{
    cout <<"StiRootDrawableHitContainer::update()"<<endl;
    const hitmap& theMap = hits();
    StiRootDrawableHits::clear();
    for (hitmap::const_iterator it=theMap.begin(); it!=theMap.end(); ++it) {
	//const hitvector& tempvec = (*it).second;
	const hitvector& tempvec = (*it).second.theHitVec;
	hitvector::const_iterator end = (*it).second.theEffectiveEnd;
	for (hitvector::const_iterator vit=tempvec.begin(); vit!=end; ++vit) {
	    const StThreeVectorF& pos = (*vit)->globalPosition();
	    add(pos.x(),pos.y(),pos.z());
	    
	    //StiRootDrawableHits::push_back( StThreeVector<double>(pos.x(), pos.y(), pos.z() ) );
	    //StiRootDrawableHits::push_back( pos.x() );
	    //StiRootDrawableHits::push_back( pos.y() );
	    //StiRootDrawableHits::push_back( pos.z() );
	    
	}
    }
    fillHitsForDrawing();
}
