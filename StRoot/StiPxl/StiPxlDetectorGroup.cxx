#include <stdexcept>

#include "StEvent/StEvent.h"
#include "StiPxl/StiPxlDetectorGroup.h"
#include "StiPxl/StiPxlDetectorBuilder.h"
#include "StiPxl/StiPxlHitLoader.h"


StiPxlDetectorGroup::StiPxlDetectorGroup(bool active,bool buildIdealGeom)
   : StiDetectorGroup<StEvent>("Pixel",
                               active ? new StiPxlHitLoader() : 0,
                               new StiPxlDetectorBuilder(active,buildIdealGeom))
{}

StiPxlDetectorGroup::~StiPxlDetectorGroup()
{}

