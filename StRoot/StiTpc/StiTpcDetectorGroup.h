#ifndef StiTpcDetectorGroup_H_INCLUDED
#define StiTpcDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiTpcDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
  StiTpcDetectorGroup(bool active, const string & inputFile);
  ~StiTpcDetectorGroup();
};

#endif

