#ifndef StiSvtHitLoader_H
#define StiSvtHitLoader_H

#include "Sti/StiHitLoader.h"
class StEvent;
class StiDetectorBuilder;

/*! \class StiSvtHitLoader
  StiSvtHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the TPC are converted using the 
  StiDetectorBuilder class.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne) and M.L. Miller (Yale Software)
 */
class StiSvtHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder>
{
public:

	StiSvtHitLoader();
	StiSvtHitLoader(StiHitContainer      * hitContainer,
			Factory<StiHit>      * hitFactory,
			StiDetectorBuilder   * detector);
	virtual ~StiSvtHitLoader();
	virtual void loadHits(StEvent* source);
};

#endif
