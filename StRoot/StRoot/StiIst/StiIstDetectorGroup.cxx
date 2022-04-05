#include "StiIst/StiIstDetectorGroup.h"
#include "StiIst/StiIstHitLoader.h"
#include "StiIst/StiIstDetectorBuilder.h"
#include "StiIst/StiIstDetectorBuilder1.h"
#include "StEvent/StEvent.h"


StiIstDetectorGroup::StiIstDetectorGroup(bool active, IstDetectorBuilderImpl istImpl, bool buildIdealGeom)
   : StiDetectorGroup<StEvent>("Ist")
{
   switch(istImpl) {
   case kFirstPro:
      _detectorBuilder = new StiIstDetectorBuilder1(active, buildIdealGeom);
      break;
   case kDefault:
   default:
      _detectorBuilder = new StiIstDetectorBuilder(active, buildIdealGeom);
   }

   _hitLoader = active ? new StiIstHitLoader() : 0;

   if (_hitLoader)
      _hitLoader->setDetector(_detectorBuilder);

   initialize();
}


StiIstDetectorGroup::~StiIstDetectorGroup()
{}
