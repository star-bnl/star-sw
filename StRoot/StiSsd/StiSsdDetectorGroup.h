#ifndef StiSsdDetectorGroup_H_INCLUDED
#define StiSsdDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;
class StiDetectorBuilder;

/*! Convenience class defining the SSD detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiSsdDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
  StiSsdDetectorGroup(bool active);
  ~StiSsdDetectorGroup();
};

#endif

