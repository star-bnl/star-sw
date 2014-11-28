#include "StiIst/StiIstDetectorGroup.h"
#include "StiIst/StiIstDetectorBuilder.h"
#include "StiIst/StiIstHitLoader.h"
#include "StEvent/StEvent.h"


StiIstDetectorGroup::StiIstDetectorGroup(bool active, bool buildIdealGeom)
   : StiDetectorGroup<StEvent>("Ist",
                               active ? new StiIstHitLoader() : 0,
                               new StiIstDetectorBuilder(active, buildIdealGeom))
{}


StiIstDetectorGroup::~StiIstDetectorGroup()
{}
