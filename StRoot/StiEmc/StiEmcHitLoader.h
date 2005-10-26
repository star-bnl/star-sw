#ifndef StiEmcHitLoader_H
#define StiEmcHitLoader_H

#include "Sti/StiHitLoader.h"
class StEvent;
class StiDetectorBuilder;

//class StEmcHit;

/*! \class StiEmcHitLoader
  StiEmcHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. 
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne State University) 
 */
class StiEmcHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder>
{
public:

    StiEmcHitLoader();
    StiEmcHitLoader(StiHitContainer * hitContainer,
		    Factory<StiHit> * hitFactory,
		    StiDetectorBuilder * detector);
    virtual ~StiEmcHitLoader();
    virtual void loadHits(StEvent* source,
			  Filter<StiTrack> * trackFilter, 
			  Filter<StiHit> * hitFilter);
};


#endif
