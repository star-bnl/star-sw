#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"
#include "StiGui/StiDetectorViews.h"

StiTpcDetectorGroup::StiTpcDetectorGroup(bool active, const string & inputFile)
  : StiDetectorGroup<StEvent,StMcEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(active,inputFile),0,0)
{}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

