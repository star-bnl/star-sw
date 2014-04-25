#include <stdexcept>

#include "StEvent/StEvent.h"
#include "StiPxl/StiPxlDetectorGroup.h"
#include "StiPxl/StiPxlDetectorBuilder.h"
#include "StiPxl/StiPxlHitLoader.h"


StiPxlDetectorGroup::StiPxlDetectorGroup(bool active, const string &inputFile)
   : StiDetectorGroup<StEvent>("Pixel",
                               active ? new StiPxlHitLoader() : 0,
                               new StiPxlDetectorBuilder(active, inputFile), 0)
{}

StiPxlDetectorGroup::~StiPxlDetectorGroup()
{}

