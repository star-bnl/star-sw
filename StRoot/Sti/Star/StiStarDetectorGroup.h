#ifndef StiStarDetectorGroup_H_INCLUDED
#define StiStarDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;

/*! Convenience class defining the STAR detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiStarDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
  StiStarDetectorGroup(bool active=false);
  ~StiStarDetectorGroup();
};

#endif
