#ifndef StiEmcHitLoader_H
#define StiEmcHitLoader_H

#include "StiHitLoader.h"
class StEvent;
class StiGeometryTransform;

/*! \class StiEmcHitLoader
  StiEmcHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the TPC are converted using the 
  StiGeometryTransform class.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne State University) 
 */
class StiEmcHitLoader : public StiHitLoader<StEvent,StiGeometryTransform>
{
public:

    StiEmcHitLoader();
    StiEmcHitLoader(StiHitContainer * hitContainer,
		    Factory<StiHit> * hitFactory,
		    StiGeometryTransform * transform);
    virtual ~StiEmcHitLoader();
    virtual void loadHits(StEvent* source);
};


#endif
