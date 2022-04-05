#ifndef StiSvtDetectorGroup_H_INCLUDED
#define StiSvtDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StiDetectorBuilder;

/*! Convenience class defining the SVT detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiSvtDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiSvtDetectorGroup(bool active);
  ~StiSvtDetectorGroup();
};

#endif

