#ifndef StiIstDetectorBuilder_h
#define StiIstDetectorBuilder_h

#include "TGeoMatrix.h"
#include "TVector3.h"

#include "Sti/StiDetectorBuilder.h"

class StIstDb;
class StiDetector;
class StiIsActiveFunctor;
class StiShape;
class StiPlacement;
class StiMaterial;


/**
 * This detector builder is responsible for constructing sensitive and inactive
 * Sti volumes describing the material of the IST detector. The corresponding
 * IST detector geometry is described in AgML files in StarVMC/Geometry/
 *
 * \author Dmitri Smirnov, BNL
 */
class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:

   StiIstDetectorBuilder(bool active, bool buildIdealGeom = true);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();
   const StiDetector* getActiveDetector(int ladder, int sensorHalf) const;

protected:

   virtual void buildInactiveVolumes();

   /// Returns a TGeo path to the sensor in the given ladder
   static std::string formTGeoPath(int ladder, int sensor);

   bool          mBuildIdealGeom;
   StIstDb      *mIstDb;
};

#endif
