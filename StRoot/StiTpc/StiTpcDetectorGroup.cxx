#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"
#include "StiGui/StiDetectorViews.h"

StiTpcDetectorGroup::StiTpcDetectorGroup(bool active, char* baseName)
  : StiDetectorGroup<StEvent,StMcEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(active, baseName),0,0)
{
  //_hitLoader->setDetector(_detectorBuilder);
}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

