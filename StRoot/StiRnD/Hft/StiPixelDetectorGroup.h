#ifndef StiPixelDetectorGroup_H_INCLUDED
#define StiPixelDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiPixelDetectorGroup : public StiDetectorGroup<StEvent,StMcEvent>
{
  public:
    StiPixelDetectorGroup(bool active, const string & inputFile);
    ~StiPixelDetectorGroup();
};

#endif

