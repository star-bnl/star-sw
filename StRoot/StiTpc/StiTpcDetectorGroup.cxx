#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"
#include "StiGui/StiDetectorViews.h"

StiTpcDetectorGroup::StiTpcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(active),0,0)
{}

StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

