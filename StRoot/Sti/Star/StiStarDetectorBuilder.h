#ifndef StiStarDetectorBuilder_H
#define StiStarDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
class StiMaterial;
class StiPlanarShape;
class StiCylindricalShape;

/*! Service class that encapsulate a builder for the basic components of 
    the star detector. These include the beam pipe and
    the interaction region.
*/
class StiStarDetectorBuilder : public StiDetectorBuilder
{
  
 public:
  StiStarDetectorBuilder();
  virtual ~StiStarDetectorBuilder(); 
  virtual void loadDb();
  virtual void buildMaterials();
  virtual void buildShapes();
  virtual void buildDetectors();
  
 protected:
  
  StiMaterial   * _pipeMaterial;
  StiMaterial   * _vacuumMaterial;
  StiCylindricalShape * _beamPipeShape;
  StiPlanarShape      * _vacuumShape;
};

#endif
