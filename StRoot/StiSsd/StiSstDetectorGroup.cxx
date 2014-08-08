#include "StiSsd/StiSstDetectorGroup.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "StiSsd/StiSstDetectorBuilder.h"
#include "Sti/StiElossCalculator.h"
#include "StEvent.h"
#include <stdexcept>

StiSstDetectorGroup::StiSstDetectorGroup(bool active, const string & inputFile)
  : StiDetectorGroup<StEvent>("SSD",
			      active?new StiSsdHitLoader():0,
			      new StiSstDetectorBuilder(active,inputFile),0)

{}

StiSstDetectorGroup::~StiSstDetectorGroup()
{}


