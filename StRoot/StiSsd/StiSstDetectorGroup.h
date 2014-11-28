#ifndef StiSstDetectorGroup_h
#define StiSstDetectorGroup_h

#include "Sti/StiDetectorGroup.h"

class StEvent;


/*!
 * Convenience class defining the SST detector group.
 *
 * \author Christelle Roy, Subatech
 * \author Dmitri Smirnov, BNL
 */
class StiSstDetectorGroup : public StiDetectorGroup<StEvent>
{
public:
   StiSstDetectorGroup(bool active, bool buildIdealGeom=false);
   ~StiSstDetectorGroup();
};

#endif
