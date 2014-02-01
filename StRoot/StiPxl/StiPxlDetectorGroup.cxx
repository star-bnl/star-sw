#include <stdexcept>
#include "StEvent.h"
#include "StiPxlDetectorGroup.h"
#include "StiPxlDetectorBuilder.h"
#include "StiPxlHitLoader.h"


StiPxlDetectorGroup::StiPxlDetectorGroup(bool active, const string &inputFile)
   : StiDetectorGroup<StEvent>("Pixel",
                               active ? new StiPxlHitLoader() : 0,
                               new StiPxlDetectorBuilder(active, inputFile), 0)
{}

StiPxlDetectorGroup::~StiPxlDetectorGroup()
{}

