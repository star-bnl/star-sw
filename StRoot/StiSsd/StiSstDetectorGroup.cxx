#include "StiSsd/StiSstDetectorGroup.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "StiSsd/StiSstDetectorBuilder.h"
#include "StEvent.h"
#include <stdexcept>

StiSstDetectorGroup::StiSstDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("SSD",
			      active?new StiSsdHitLoader():0,
			      new StiSstDetectorBuilder(active))

{}

StiSstDetectorGroup::~StiSstDetectorGroup()
{}


