#include <stdexcept>
#include "StEvent.h"
#include "StiTpc/StiTpcDetectorGroup.h"
#include "StiTpc/StiTpcDetectorBuilder.h"
#include "StiTpc/StiTpcHitLoader.h"


StiTpcDetectorGroup::StiTpcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(),0,0)
{
  //_hitLoader->setDetector(_detectorBuilder);
}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

