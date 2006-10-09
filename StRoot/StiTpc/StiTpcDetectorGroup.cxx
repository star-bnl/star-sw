#include <stdexcept>
#include "StEvent.h"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"

StiTpcDetectorGroup::StiTpcDetectorGroup(bool active, const string & inputFile)
  : StiDetectorGroup<StEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(active,inputFile),0)
{}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

