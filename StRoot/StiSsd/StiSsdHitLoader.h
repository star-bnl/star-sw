// $Id: StiSsdHitLoader.h,v 1.7.4.1 2015/01/07 19:45:34 smirnovd Exp $
// 
// $Log: StiSsdHitLoader.h,v $
// Revision 1.7.4.1  2015/01/07 19:45:34  smirnovd
// Squashed commit of changes in StiXxx/ from MAIN CVS branch:
//
// Date:   Tue Jan 6 20:57:50 2015 +0000
//
//     Reimplemented segmentation of PXL sensor to two halves.
//
//     In the sensor's local coordinate system the first half is for x<0 and the second
//     one is for x>0. The notion of inner and outter halves is not critical and in
//     fact confusing because it depends on the original rotation around the z axis.
//     For example, two rotations of phi=5 and phi=-175 give us essentially the same
//     layer but result in swapped inner and outter halves.
//
// Date:   Tue Jan 6 15:48:08 2015 +0000
//
//     StiIstHitLoader: Made use of the accessor for sensitive Sti detector/volumes
//
// Date:   Tue Jan 6 15:48:04 2015 +0000
//
//     StiXxxDetectorBuilder: Added an accessor to access active StiDetectors, i.e. volumes which may have hits associated with them
//
// Date:   Tue Jan 6 15:47:58 2015 +0000
//
//     StiPxlDetectorBuilder: Switched to method that converts geo sensor id to Sti layer indices
//
// Date:   Tue Jan 6 15:47:50 2015 +0000
//
//     StiPxlDetectorBuilder: Added a private method to convert natural/geo sensor id to Sti layer indices
//
// Date:   Tue Jan 6 15:47:43 2015 +0000
//
//     Removed excessive print statements
//
// Date:   Tue Jan 6 15:47:34 2015 +0000
//
//     Simplified debug output by reusing existing streamers of StHit class and its daughters
//
// Date:   Mon Jan 5 15:40:30 2015 +0000
//
//     StiIstHitLoader: Cleaned up forward declarations to include only the used classes
//
// Date:   Mon Jan 5 15:40:03 2015 +0000
//
//     StiXxxHitLoader: Changes in whitespace only
//
// Date:   Mon Jan 5 15:39:52 2015 +0000
//
//     StiIstHitLoader: Removed useless data members
//
// Date:   Fri Dec 19 18:09:01 2014 +0000
//
//     Do not set StiDetector members _key1 and _key2 as they are not really used anywhere
//
// Date:   Fri Dec 19 18:08:52 2014 +0000
//
//     StiXxxDetectorBuilder: Removed output debug messages as they can be easily replaced by a single call to StiDetectorBuilder::Print()
//
// Date:   Fri Dec 19 18:08:40 2014 +0000
//
//     StiXxxDetectorBuilder: Instead of setting StiDetector parameters in a local private method switched to using new interface provided by StiDetector
//
//     The refactoring takes place for both sensitive and inactive volumes
//
// Date:   Mon Dec 15 22:18:19 2014 +0000
//
//     StiPxlDetectorBuilder: Attempted to make a clear translation between the natural (sector/ladder/sensor) and Sti numbering schemas
//
// Date:   Mon Dec 15 22:18:10 2014 +0000
//
//     StiIstDetectorBuilder: Increased density of manually constructed IST brackets in Sti.
//
//     The effective bracket density have to be muliplied by the number of ladders (24)
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
