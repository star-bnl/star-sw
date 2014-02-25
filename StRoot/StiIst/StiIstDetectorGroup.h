#ifndef StiIstDetectorGroup_h
#define StiIstDetectorGroup_h

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
   StiIstDetectorGroup(bool active, const string &inputFile);
   ~StiIstDetectorGroup();
};

#endif
