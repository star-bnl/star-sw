#include <stdexcept>
#include "StEvent.h"
#include "StiPixelDetectorGroup.h"
#include "StiPixelDetectorBuilder.h"
#include "StiPixelHitLoader.h"


StiPixelDetectorGroup::StiPixelDetectorGroup(bool active)
    : StiDetectorGroup<StEvent>("Pixel",
					  active?new StiPixelHitLoader():0,
					  new StiPixelDetectorBuilder(active))
{}

StiPixelDetectorGroup::~StiPixelDetectorGroup()
{}

