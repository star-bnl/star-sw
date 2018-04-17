#include <stdexcept>
#include "StEvent.h"
#include "StiTpcDetectorGroup.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcHitLoader.h"

StiTpcDetectorGroup::StiTpcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("TPC",
			      active?new StiTpcHitLoader():0,
			      new StiTpcDetectorBuilder(active))
{}


/** Constructor to control TPC hit usage in Sti tracking */
StiTpcDetectorGroup::StiTpcDetectorGroup(bool active_Tpc, bool active_iTpc)
  : StiDetectorGroup<StEvent>("TPC",
                              active_Tpc ? new StiTpcHitLoader() : nullptr,
                              new StiTpcDetectorBuilder(active_Tpc, active_iTpc))
{}


StiTpcDetectorGroup::~StiTpcDetectorGroup()
{}

