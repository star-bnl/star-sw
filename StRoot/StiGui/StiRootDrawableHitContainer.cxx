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
  cout <<"StiRootDrawableHitContainer::~StiRootDrawableHitContainer() -I- Done"<<endl;
}

void StiRootDrawableHitContainer::clear()
{
  StiRootDrawableHits::clear();
  StiHitContainer::clear();
}

void StiRootDrawableHitContainer::update()
{
    cout <<"StiRootDrawableHitContainer::update() -I- Started"<<endl;
    const hitmap& theMap = hits();
    StiRootDrawableHits::clear();
    cout << "StiRootDrawableHitContainer::update() -I- Map Size:" << theMap.size()<<endl;
    for (hitmap::const_iterator it=theMap.begin(); it!=theMap.end(); ++it) 
      {
	const hitvector& tempvec = (*it).second.theHitVec;
	hitvector::const_iterator end = (*it).second.theEffectiveEnd;
	for (hitvector::const_iterator vit=tempvec.begin(); vit!=end; ++vit) 
	  {
	    const StThreeVectorF& pos = (*vit)->globalPosition();
	    add(pos.x(),pos.y(),pos.z());
	  }
      }
    fillHitsForDrawing();
    cout <<"StiRootDrawableHitContainer::update() -I- Done"<<endl;
}
