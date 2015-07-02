#ifndef StiEmcDetectorGroup_H_INCLUDED
#define StiEmcDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"

class StEvent;
class StiDetectorBuilder;

/*! Convenience class defining the EMC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiEmcDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiEmcDetectorGroup(bool active);
  ~StiEmcDetectorGroup();
};

#endif

