#include "StiEmc/StiEmcDetectorGroup.h"
#include "StiEmc/StiEmcDetectorBuilder.h"
#include "StiEmc/StiEmcHitLoader.h"
#include "Sti/StiDetectorBuilder.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include <stdexcept>

StiEmcDetectorGroup::StiEmcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("EMC",
			      active?new StiEmcHitLoader():0,
			      new StiEmcDetectorBuilder(),
			      0,
			      0)
{}

StiEmcDetectorGroup::~StiEmcDetectorGroup()
{}


