#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiPixelDetectorGroup.h"
#include "StiPixelDetectorBuilder.h"
#include "StiPixelHitLoader.h"


StiPixelDetectorGroup::StiPixelDetectorGroup(bool active)
    : StiDetectorGroup<StEvent,StMcEvent>("Pixel",
					  active?new StiPixelHitLoader():0,
					  new StiPixelDetectorBuilder(),0,0)
{
    //_hitLoader->setDetector(_detectorBuilder);
}

StiPixelDetectorGroup::~StiPixelDetectorGroup()
{}

