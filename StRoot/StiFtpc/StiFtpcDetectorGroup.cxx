#include "StiFtpc/StiFtpcDetectorGroup.h"
#include "StiFtpc/StiFtpcDetectorBuilder.h"
#include "StiFtpc/StiFtpcHitLoader.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDedxCalculator.h"
#include "Sti/StiElossCalculator.h"
#include <stdexcept>

StiFtpcDetectorGroup::StiFtpcDetectorGroup(bool active, const string & inputFile)
  : StiDetectorGroup<StEvent,StMcEvent>("FTPC",
			      active?new StiFtpcHitLoader():0,
			      new StiFtpcDetectorBuilder(active,inputFile),0,0)
{}


StiFtpcDetectorGroup::~StiFtpcDetectorGroup()
{}


