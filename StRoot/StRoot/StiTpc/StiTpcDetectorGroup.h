#ifndef StiTpcDetectorGroup_H_INCLUDED
#define StiTpcDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiTpcDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiTpcDetectorGroup(bool active);
  StiTpcDetectorGroup(bool active_Tpc, bool active_iTpc);
  ~StiTpcDetectorGroup();
};

#endif

