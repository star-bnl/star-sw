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

   /// Options to identify different SST detector builders
   enum SstDetectorBuilderImpl {kDefault, kFirstPro};

   StiSstDetectorGroup(bool active, SstDetectorBuilderImpl sstImpl=kDefault, bool buildIdealGeom=false);
   ~StiSstDetectorGroup();
};

#endif
