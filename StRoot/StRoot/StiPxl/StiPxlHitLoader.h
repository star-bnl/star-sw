#ifndef StiPxlHitLoader_h
#define StiPxlHitLoader_h

#include "Sti/StiHitLoader.h"

class StEvent;
class StiDetectorBuilder;


/*! \class StiPxlHitLoader
StiPxlHitLoader is a concrete class implementing the StiHitLoader abstract
interface. It is used to load hits from Star StEvent into the StiHitContainer
for Sti tracking. StEvent hits from the TPC are converted using the
StiPxlDetectorBuilder methods.
<p>
This class is substantially morphed from the class StiHitFiller
originally written by Mike Miller.
\author Claude A Pruneau (Wayne)
*/
class StiPxlHitLoader : public StiHitLoader<StEvent, StiDetectorBuilder>
{
public:

   StiPxlHitLoader();
   StiPxlHitLoader(StiHitContainer *hitContainer, Factory<StiHit> *hitFactory, StiDetectorBuilder *detector);
   virtual void loadHits(StEvent *source, Filter<StiTrack> *trackFilter, Filter<StiHit> *hitFilter);
};

#endif
