#include "StiEmc/StiEmcDetectorGroup.h"
#include "StiEmc/StiEmcDetectorBuilder.h"
#include "StiEmc/StiEmcHitLoader.h"
#include "Sti/StiDetectorBuilder.h"
#include "StEvent.h"
#include <stdexcept>

StiEmcDetectorGroup::StiEmcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("EMC",
			      active?new StiEmcHitLoader():0,
			      new StiEmcDetectorBuilder(active))
{}

StiEmcDetectorGroup::~StiEmcDetectorGroup()
{}


