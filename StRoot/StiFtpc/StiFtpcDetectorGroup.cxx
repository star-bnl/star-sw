#include "StiFtpc/StiFtpcDetectorGroup.h"
#include "StiFtpc/StiFtpcDetectorBuilder.h"
#include "StiFtpc/StiFtpcHitLoader.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiDedxCalculator.h"
#include "Sti/StiElossCalculator.h"
#include <stdexcept>

StiFtpcDetectorGroup::StiFtpcDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("FTPC",
			      active?new StiFtpcHitLoader():0,
			      new StiFtpcDetectorBuilder(),0,0)
{}


StiFtpcDetectorGroup::~StiFtpcDetectorGroup()
{}


