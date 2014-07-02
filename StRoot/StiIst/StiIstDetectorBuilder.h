#ifndef StiIstDetectorBuilder_h
#define StiIstDetectorBuilder_h

#include "Sti/StiDetectorBuilder.h"

class StIstDbMaker;
class StiPlanarShape;
class StiCylindricalShape;
class StiMaterial;
class StiPlacement;
class StiElossCalculator;

class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:

   StiIstDetectorBuilder(bool active, const string &inputFile, bool buildIdealGeom=true);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   StiMaterial  *mSiMaterial;
   StiMaterial  *mHybridMaterial;
   StIstDbMaker *mIstDb;
   bool          mBuildIdealGeom;

private:

   void buildInactiveVolumes();
   void buildPlanerVolume(StiDetector& detector, string detName, float halfDepth, float thickness, float halfWidth, float yShift, float rShift, float zShift, StiPlacement *placement, StiMaterial *mat, StiElossCalculator *elossCalculator);
   void buildTubeVolume(StiDetector& detector, string detName, float halfDepth, float thickness, float outerRadius, float openingAngle, float zCenter, StiMaterial *mat, StiElossCalculator *elossCalculator);
};

#endif
