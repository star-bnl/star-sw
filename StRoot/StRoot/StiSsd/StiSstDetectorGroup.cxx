#include "StiSsd/StiSstDetectorGroup.h"
#include "StiSsd/StiSstHitLoader.h"
#include "StiSsd/StiSstDetectorBuilder.h"
#include "StiSsd/StiSstDetectorBuilder1.h"
#include "StEvent/StEvent.h"


StiSstDetectorGroup::StiSstDetectorGroup(bool active, SstDetectorBuilderImpl sstImpl, bool buildIdealGeom)
  : StiDetectorGroup<StEvent>("SST")
{
   switch(sstImpl) {
   case kFirstPro:
      _detectorBuilder = new StiSstDetectorBuilder1(active, buildIdealGeom);
      break;
   case kDefault:
   default:
      _detectorBuilder = new StiSstDetectorBuilder(active, buildIdealGeom);
   }

   _hitLoader = active ? new StiSstHitLoader() : 0;

   if (_hitLoader)
      _hitLoader->setDetector(_detectorBuilder);

   initialize();
}


StiSstDetectorGroup::~StiSstDetectorGroup()
{}
