#ifndef StiFtpcHitLoader_H
#define StiFtpcHitLoader_H

#include "Sti/StiHitLoader.h"
class StEvent;
class StiDetectorBuilder;

/*! \class StiFtpcHitLoader
  StiFtpcHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the FTPC are converted using the 
  StiDetectorBuilder class.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne State Univ) 
 */
class StiFtpcHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder>
{
public:

    StiFtpcHitLoader();
    StiFtpcHitLoader(StiHitContainer * hitContainer,
		     Factory<StiHit> * hitFactory,
		     StiDetectorBuilder * transform);
    virtual ~StiFtpcHitLoader();
    virtual void loadHits(StEvent* source);
};


#endif
