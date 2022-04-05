#ifndef StiSstDetectorBuilder1_H
#define StiSstDetectorBuilder1_H

#include "StiSsd/StiSstDetectorBuilder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StSsdDbMaker/StSstDbMaker.h"

class StiDetector;

class StiSstDetectorBuilder1 : public StiSstDetectorBuilder
{

public:

   StiSstDetectorBuilder1(bool active, bool buildIdealGeom=true);
   virtual ~StiSstDetectorBuilder1();

protected:

   virtual void buildInactiveVolumes();

private:

   void segmentSFMOVolume(StiDetector* stiSFMO);
};
#endif
