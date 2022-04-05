#ifndef StiStarDetectorGroup_H_INCLUDED
#define StiStarDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;

/*! Convenience class defining the STAR detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiStarDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiStarDetectorGroup(bool active);
  ~StiStarDetectorGroup();
};

#endif
