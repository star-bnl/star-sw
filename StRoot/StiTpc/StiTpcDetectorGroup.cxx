#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"


StiTpcDetectorGroup::StiTpcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(),0,0)
{
  //_hitLoader->setDetector(_detectorBuilder);
}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

