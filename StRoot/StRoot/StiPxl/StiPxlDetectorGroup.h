#ifndef StiPxlDetectorGroup_h
#define StiPxlDetectorGroup_h

#include "Sti/StiDetectorGroup.h"

class StEvent;


/*!
 * Convenience class defining the PXL detector group.
 *
 * \author Claude A Pruneau, Wayne State University
 * \author Dmitri Smirnov, BNL
 */
class StiPxlDetectorGroup : public StiDetectorGroup<StEvent>
{
public:
   StiPxlDetectorGroup(bool active, bool buildIdealGeom=false);
   ~StiPxlDetectorGroup();
};

#endif
