#include <stdexcept>
#include "StEvent.h"
#include "StMcEvent.hh"
#include "StiHpdDetectorGroup.h"
#include "StiHpdDetectorBuilder.h"
#include "StiHpdHitLoader.h"


StiHpdDetectorGroup::StiHpdDetectorGroup(bool active, const string & inputFile)
    : StiDetectorGroup<StEvent>("Hpd",
					  active?new StiHpdHitLoader():0,
					  new StiHpdDetectorBuilder(active,inputFile),0)
{}

StiHpdDetectorGroup::~StiHpdDetectorGroup()
{}

