#ifndef StiSvtDetectorGroup_H_INCLUDED
#define StiSvtDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;
class StiDetectorBuilder;

/*! Convenience class defining the SVT detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiSvtDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
  StiSvtDetectorGroup(bool active, char* baseName);
  ~StiSvtDetectorGroup();
};

#endif

