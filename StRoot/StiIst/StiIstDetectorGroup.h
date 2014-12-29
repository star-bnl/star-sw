#ifndef StiIstDetectorGroup_h
#define StiIstDetectorGroup_h

#include "Sti/StiDetectorGroup.h"

class StEvent;


/*!
 * Convenience class defining the IST detector group.
 *
 * \author Claude A Pruneau, Wayne State University
 * \author Dmitri Smirnov, BNL
 */
class StiIstDetectorGroup : public StiDetectorGroup<StEvent>
{
public:

   /// Options to identify different IST detector builders
   enum IstDetectorBuilderImpl {kDefault, kFirstPro};

   StiIstDetectorGroup(bool active, IstDetectorBuilderImpl istImpl=kDefault, bool buildIdealGeom=false);
   ~StiIstDetectorGroup();
};

#endif
