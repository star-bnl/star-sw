#ifndef StiPixelDetectorGroup_H_INCLUDED
#define StiPixelDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiPixelDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
    StiPixelDetectorGroup(bool active);
    ~StiPixelDetectorGroup();
};

#endif

