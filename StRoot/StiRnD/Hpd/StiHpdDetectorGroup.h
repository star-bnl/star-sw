#ifndef StiHpdDetectorGroup_H_INCLUDED
#define StiHpdDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StMcEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiHpdDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
    StiHpdDetectorGroup(bool active, const string & inputFile);
    ~StiHpdDetectorGroup();
};

#endif

