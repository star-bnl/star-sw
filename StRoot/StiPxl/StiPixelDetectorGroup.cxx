#include <stdexcept>
#include "StEvent.h"
#include "StiPixelDetectorGroup.h"
#include "StiPixelDetectorBuilder.h"
#include "StiPixelHitLoader.h"


StiPixelDetectorGroup::StiPixelDetectorGroup(bool active, const string &inputFile)
   : StiDetectorGroup<StEvent>("Pixel",
                               active ? new StiPixelHitLoader() : 0,
                               new StiPixelDetectorBuilder(active, inputFile), 0)
{}

StiPixelDetectorGroup::~StiPixelDetectorGroup()
{}

