#include "StiSsd/StiSsdDetectorGroup.h"
#include "StiSsd/StiSsdHitLoader.h"
#include "StiSsd/StiSsdDetectorBuilder.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDedxCalculator.h"
#include "Sti/StiElossCalculator.h"
#include "StEvent.h"
#include <stdexcept>

StiSsdDetectorGroup::StiSsdDetectorGroup(bool active, const string & inputFile)
  : StiDetectorGroup<StEvent,StMcEvent>("SSD",
			      active?new StiSsdHitLoader():0,
			      new StiSsdDetectorBuilder(active,inputFile),0,0)
{}

StiSsdDetectorGroup::~StiSsdDetectorGroup()
{}


