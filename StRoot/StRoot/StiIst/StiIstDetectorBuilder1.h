#ifndef StiIstDetectorBuilder1_h
#define StiIstDetectorBuilder1_h

#include "StiIst/StiIstDetectorBuilder.h"

class StiMaterial;
class StiPlacement;


class StiIstDetectorBuilder1 : public StiIstDetectorBuilder
{
public:

   StiIstDetectorBuilder1(bool active, bool buildIdealGeom = true);

protected:

   virtual void buildInactiveVolumes();

private:

   void buildPlanerVolume(StiDetector& detector, std::string detName, float halfDepth, float thickness, float halfWidth,
         float yShift, float rShift, float zShift, StiPlacement *placement, StiMaterial *mat);
   void buildTubeVolume(StiDetector& detector, std::string detName, float halfDepth, float thickness,
         float outerRadius, float openingAngle, float zCenter, StiMaterial *mat);
};

#endif
