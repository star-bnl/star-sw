#ifndef StiSstDetectorGroup_H_INCLUDED
#define StiSstDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StiDetectorBuilder;

/*! Convenience class defining the SST detector group
  <p>
  \author Christelle Roy, Subatech
*/
class StiSstDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiSstDetectorGroup(bool active, const string & inputFile);
  ~StiSstDetectorGroup();
};

#endif

