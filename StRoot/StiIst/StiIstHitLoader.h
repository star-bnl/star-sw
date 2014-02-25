#ifndef StiIstHitLoader_h
#define StiIstHitLoader_h

#include "Sti/StiHitLoader.h"

#include "StEvent.h"
class StMcEvent;
class StMcTrack;
class StiMcTrack;
class StiDetectorBuilder;
class StTpcHit;
class StMcIstHit;


/*!
StiIstHitLoader is a concrete class implementing the StiHitLoader abstract
interface. It is used to load hits from Star StEvent into the StiHitContainer
for Sti tracking. StEvent hits from the TPC are converted using the
StiIstDetectorBuilder methods.

This class is substantially morphed from the class StiHitFiller originally
written by Mike Miller.

\author Claude A Pruneau (Wayne)
\author Yaping Wang (UIC)
*/
class StiIstHitLoader : public StiHitLoader<StEvent, StiDetectorBuilder>
{
public:
   StiIstHitLoader();
   StiIstHitLoader(StiHitContainer *hitContainer,
                   Factory<StiHit> *hitFactory,
                   StiDetectorBuilder *detector);
   virtual ~StiIstHitLoader();
   virtual void loadHits(StEvent *source,
                         Filter<StiTrack> *trackFilter,
                         Filter<StiHit> *hitFilter);

protected:
   // temporary hit ptr used to determine whether mc hits from a given event are
   // already loaded.
   UInt_t n;
   StMcIstHit *saveHit;
   long evNum;
};

#endif
