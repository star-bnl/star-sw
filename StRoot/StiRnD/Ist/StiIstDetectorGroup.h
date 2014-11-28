#ifndef StiIstDetectorGroup_H_INCLUDED
#define StiIstDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiIstDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
    StiIstDetectorGroup(bool active);
    ~StiIstDetectorGroup();
};

#endif

