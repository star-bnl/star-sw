#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiSvt/StiSvtDetectorBuilder.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "StEvent.h"
#include "StMcEvent.hh"
#include "Sti/StiDedxCalculator.h"
#include "Sti/StiElossCalculator.h"
#include <stdexcept>

StiSvtDetectorGroup::StiSvtDetectorGroup(bool active)
  : StiDetectorGroup<StEvent,StMcEvent>("SVT",
					active?new StiSvtHitLoader():0,
					new StiSvtDetectorBuilder(active),0,0)
{}

StiSvtDetectorGroup::~StiSvtDetectorGroup()
{}


