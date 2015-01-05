// $Id: StiSsdHitLoader.h,v 1.8 2015/01/05 15:40:04 smirnovd Exp $
// 
// $Log: StiSsdHitLoader.h,v $
// Revision 1.8  2015/01/05 15:40:04  smirnovd
// StiXxxHitLoader: Changes in whitespace only
//
// Revision 1.7  2005/10/26 21:59:12  fisyak
// get rid off dependencies from StMcEvent
//
// Revision 1.6  2005/06/21 15:31:48  lmartin
// CVS tags added
//
/*!
 * \author Christelle Roy
*/
#ifndef StiSsdHitLoader_H
#define StiSsdHitLoader_H

#include "Sti/StiHitLoader.h"

class StEvent;
class StiDetectorBuilder;


/*! \class StiSsdHitLoader
  StiSsdHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the TPC are converted using the 
  StiDetectorBuilder class.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne) and M.L. Miller (Yale Software)
 */
class StiSsdHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder>
{
public:

    StiSsdHitLoader();
    StiSsdHitLoader(StiHitContainer* hitContainer, Factory<StiHit>* hitFactory, StiDetectorBuilder* detector);
    virtual ~StiSsdHitLoader();
    virtual void loadHits(StEvent *source, Filter<StiTrack> *trackFilter, Filter<StiHit> *hitFilter);
};

#endif
