// $Id: StiSsdDetectorGroup.h,v 1.6 2005/10/26 21:59:12 fisyak Exp $
// 
// $Log: StiSsdDetectorGroup.h,v $
// Revision 1.6  2005/10/26 21:59:12  fisyak
// get rid off dependencies from StMcEvent
//
// Revision 1.5  2005/06/21 15:31:47  lmartin
// CVS tags added
//
#ifndef StiSsdDetectorGroup_H_INCLUDED
#define StiSsdDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StiDetectorBuilder;

/*! Convenience class defining the SSD detector group
  <p>
  \author Christelle Roy, Subatech
*/
class StiSsdDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
  StiSsdDetectorGroup(bool active, const string & inputFile);
  ~StiSsdDetectorGroup();
};

#endif

