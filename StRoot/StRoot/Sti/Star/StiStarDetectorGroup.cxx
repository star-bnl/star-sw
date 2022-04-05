#include <stdexcept>
#include "StEvent.h"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "Sti/Star/StiStarDetectorBuilder.h"
#include "Sti/StiHitLoader.h"

StiStarDetectorGroup::StiStarDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("STAR",
			      0,
			      new StiStarDetectorBuilder(active))
{}

StiStarDetectorGroup::~StiStarDetectorGroup()
{}

