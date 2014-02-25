
#include "StEvent.h"
#include "StiIstDetectorGroup.h"
#include "StiIstDetectorBuilder.h"
#include "StiIstHitLoader.h"


StiIstDetectorGroup::StiIstDetectorGroup(bool active, const string &inputFile)
   : StiDetectorGroup<StEvent>("Ist",
                               active ? new StiIstHitLoader() : 0,
                               new StiIstDetectorBuilder(active, inputFile), 0)
{}

StiIstDetectorGroup::~StiIstDetectorGroup()
{}
