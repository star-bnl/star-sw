#ifndef StiIstHitLoader_h
#define StiIstHitLoader_h

#include "Sti/StiHitLoader.h"

class StEvent;
class StiDetectorBuilder;


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
   StiIstHitLoader(StiHitContainer *hitContainer, Factory<StiHit> *hitFactory, StiDetectorBuilder *detector);
   virtual ~StiIstHitLoader();
   virtual void loadHits(StEvent *source, Filter<StiTrack> *trackFilter, Filter<StiHit> *hitFilter);
};

#endif
