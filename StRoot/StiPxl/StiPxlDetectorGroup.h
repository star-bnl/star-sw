#ifndef StiPxlDetectorGroup_H_INCLUDED
#define StiPxlDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;

/*! Convenience class defining the TPC detector group
  <p>
  \author Claude A Pruneau, Wayne State University
*/
class StiPxlDetectorGroup : public StiDetectorGroup<StEvent>
{
public:
   StiPxlDetectorGroup(bool active, const string &inputFile, bool buildIdealGeom=false);
   ~StiPxlDetectorGroup();
};

#endif

