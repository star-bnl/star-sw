#ifndef StiStarDetectorBuilder_H
#define StiStarDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"

/// This service class encapsulates a builder for the basic components of 
/// the star detector. These components include the beam pipe and
/// the interaction region.
class StiStarDetectorBuilder : public StiDetectorBuilder
{
 public:
  StiStarDetectorBuilder(bool active, const string & inputFile);
  virtual ~StiStarDetectorBuilder(); 
  virtual void buildDetectors(StMaker&s);	
 protected:
  StiMaterial         * _pipeMaterial;
  StiMaterial         * _vacuumMaterial;
  StiCylindricalShape * _beamPipeShape;
  StiPlanarShape      * _vacuumShape;
};

#endif
