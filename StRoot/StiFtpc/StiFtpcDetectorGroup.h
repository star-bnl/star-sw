#ifndef StiFtpcDetectorGroup_H_INCLUDED
#define StiFtpcDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;
class StiDetectorBuilder;

/*! Convenience class defining the FTPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiFtpcDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
  StiFtpcDetectorGroup(bool active);
  ~StiFtpcDetectorGroup();
};

#endif

