#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiPixelDetectorGroup.h"
#include "StiPixelDetectorBuilder.h"
#include "StiPixelHitLoader.h"


StiPixelDetectorGroup::StiPixelDetectorGroup(bool active, const string & inputFile)
    : StiDetectorGroup<StEvent,StMcEvent>("Pixel",
					  active?new StiPixelHitLoader():0,
					  new StiPixelDetectorBuilder(active,inputFile),0,0)
{}

StiPixelDetectorGroup::~StiPixelDetectorGroup()
{}

