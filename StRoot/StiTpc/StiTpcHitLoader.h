#ifndef StiTpcHitLoader_H
#define StiTpcHitLoader_H
#include "Sti/StiHitLoader.h"

class StEvent;
class StMcEvent;
class StiDetectorBuilder;
class StTpcHit;

/*! \class StiTpcHitLoader
  StiTpcHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the TPC are converted using the 
  StiTpcDetectorBuilder methods.
  <p>
  This class is substantially morphed from the class StiHitFiller 
  originally written by Mike Miller.
  \author Claude A Pruneau (Wayne) 
 */
class StiTpcHitLoader : public StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>
{
 public:
  StiTpcHitLoader();
  StiTpcHitLoader(StiHitContainer * hitContainer,
		  StiHitContainer * mcHitContainer,
		  Factory<StiHit> * hitFactory,
		  StiDetectorBuilder * detector);
  virtual ~StiTpcHitLoader();
  virtual void loadHits(StEvent* source,
			Filter<StiTrack> * trackFilter, 
			Filter<StiHit> * hitFilter);
  virtual void loadMcHits(StMcEvent* source,bool useMcAsRec,
			  Filter<StiTrack> * trackFilter, 
			  Filter<StiHit> * hitFilter);
};

#endif
