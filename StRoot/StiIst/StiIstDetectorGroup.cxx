
#include "StEvent.h"
#include "StiIstDetectorGroup.h"
#include "StiIstDetectorBuilder.h"
#include "StiIstHitLoader.h"


StiIstDetectorGroup::StiIstDetectorGroup(bool active, bool buildIdealGeom)
   : StiDetectorGroup<StEvent>("Ist",
                               active ? new StiIstHitLoader() : 0,
                               new StiIstDetectorBuilder(active, buildIdealGeom))
{}

StiIstDetectorGroup::~StiIstDetectorGroup()
{}
