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


class StiIstDetectorBuilder : public StiDetectorBuilder
{
public:

   StiIstDetectorBuilder(bool active, bool buildIdealGeom = true);
   virtual void buildDetectors(StMaker &source);
   virtual void useVMCGeometry();

protected:

   virtual void buildInactiveVolumes();

   bool          mBuildIdealGeom;
   StIstDb      *mIstDb;

private:

   static void setDetectorProperties(StiDetector* detector, std::string name, StiIsActiveFunctor* activeFunctor, StiShape* shape, StiPlacement* placement, StiMaterial* gas, StiMaterial* material);
   static StiPlacement* createPlacement(const TGeoMatrix& transMatrix, const TVector3& localCenterOffset=TVector3(), const TVector3& normal=TVector3(0, 1, 0));
};

#endif
