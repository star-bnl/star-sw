#ifndef StiPixelHitLoader_H
#define StiPixelHitLoader_H
#include "Sti/StiHitLoader.h"

class StEvent;
class StMcEvent;
class StiDetectorBuilder;
class StTpcHit;

/*! \class StiPixelHitLoader
StiPixelHitLoader is a concrete class implementing the StiHitLoader abstract
interface. It is used to load hits from Star StEvent into the StiHitContainer
for Sti tracking. StEvent hits from the TPC are converted using the
StiPixelDetectorBuilder methods.
<p>
This class is substantially morphed from the class StiHitFiller
originally written by Mike Miller.
\author Claude A Pruneau (Wayne)
*/
class StiPixelHitLoader : public StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>
{
public:
  StiPixelHitLoader();
  StiPixelHitLoader(StiHitContainer * hitContainer,
                    StiHitContainer * mcHitContainer,
                    Factory<StiHit> * hitFactory,
                    StiDetectorBuilder * detector);
  virtual ~StiPixelHitLoader();
  virtual void loadHits(StEvent* source,
                        Filter<StiTrack> * trackFilter,
                        Filter<StiHit> * hitFilter);
  virtual void loadMcHits(StMcEvent* source,bool useMcAsRec,
                          Filter<StiTrack> * trackFilter,
                          Filter<StiHit> * hitFilter,
                          StMcTrack & stMcTrack,
                          StiMcTrack & stiMcTrack);

 protected:
  // temporary hit ptr used to determine whether mc hits from a given event are
  // already loaded.
  StMcPixelHit * saveHit;
};

#endif
