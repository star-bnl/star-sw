#include "StiSvt/StiSvtDetectorGroup.h"
#include "StiSvt/StiSvtDetectorBuilder.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "StEvent.h"
#include "Sti/StiElossCalculator.h"
#include <stdexcept>

StiSvtDetectorGroup::StiSvtDetectorGroup(bool active)
  : StiDetectorGroup<StEvent>("SVT",
					active?new StiSvtHitLoader():0,
					new StiSvtDetectorBuilder(active))
{}

StiSvtDetectorGroup::~StiSvtDetectorGroup()
{}


