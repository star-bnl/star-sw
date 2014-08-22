// $Id: StiSsdDetectorGroup.cxx,v 1.9 2014/08/22 17:53:32 perev Exp $
// 
// $Log: StiSsdDetectorGroup.cxx,v $
// Revision 1.9  2014/08/22 17:53:32  perev
// Remove never used input file
//
// Revision 1.8  2006/10/09 15:47:59  fisyak
// use Normal represantation, remove StiDedxCalculator
//
// Revision 1.7  2005/10/26 21:59:12  fisyak
// get rid off dependencies from StMcEvent
//
// Revision 1.6  2005/06/21 15:31:47  lmartin
// CVS tags added
//
#include "StiSsd/StiSsdDetectorGroup.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "StiSsd/StiSsdDetectorBuilder.h"
#include "StEvent.h"
#include <stdexcept>

StiSsdDetectorGroup::StiSsdDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("SSD",
			      active?new StiSsdHitLoader():0,
			      new StiSsdDetectorBuilder(active))

{}

StiSsdDetectorGroup::~StiSsdDetectorGroup()
{}


