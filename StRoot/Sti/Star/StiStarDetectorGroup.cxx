#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/Star/StiStarDetectorGroup.h"
#include "Sti/Star/StiStarDetectorBuilder.h"
#include "Sti/StiHitLoader.h"

StiStarDetectorGroup::StiStarDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("STAR",
			      0,
			      new StiStarDetectorBuilder(active),
			      0,
			      0)
{}

StiStarDetectorGroup::~StiStarDetectorGroup()
{}

