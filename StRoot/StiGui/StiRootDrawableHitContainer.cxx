//StiRootDrawableHitContainer.cxx
//M.L. Miller (Yale Software)
//09/01

//StiGui
#include "StiDisplayManager.h"
#include "StiRootDrawableHitContainer.h"

StiRootDrawableHitContainer::StiRootDrawableHitContainer()
{
    cout <<"StiRootDrawableHitContainer::StiRootDrawableHitContainer()"<<endl;
    setMarkerSize(.5);
    setName("StiRootDrawableHitContainer");
    StiDisplayManager::instance()->addDrawable(this);
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
    
    for (hitmap::const_iterator it=theMap.begin(); it!=theMap.end(); it++) {
	const hitvector& tempvec = (*it).second;
	for (hitvector::const_iterator vit=tempvec.begin(); vit!=tempvec.end(); vit++) {
	    StiRootDrawableHits::push_back( *vit );
	}
    }
    fillHitsForDrawing();
}
