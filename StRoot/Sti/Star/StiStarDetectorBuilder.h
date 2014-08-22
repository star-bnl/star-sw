#ifndef StiStarDetectorBuilder_H
#define StiStarDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"

/// This service class encapsulates a builder for the basic components of 
/// the star detector. These components include the beam pipe and
/// the interaction region.
class VolumeMap_t;
class StiStarDetectorBuilder : public StiDetectorBuilder
{
 public:
  StiStarDetectorBuilder(bool active) : StiDetectorBuilder("StarBuilder",active), 
    _pipeMaterial(0), _vacuumMaterial(0), _beamPipeShape(0), _vacuumShape(0), _TpcRefSys(kFALSE) {}
  virtual ~StiStarDetectorBuilder() {}
  virtual void buildDetectors(StMaker&s);	
  void         useVMCGeometry();
  void         OldBeamPipe();
  void         HftBeamPipe();
  void         NewSuppCone();
  void         Fgt();
  void         MakePipe(Int_t iflag, const VolumeMap_t *ptube, const VolumeMap_t *pvacu);
 protected:
  StiMaterial         * _pipeMaterial;
  StiMaterial         * _vacuumMaterial;
  StiCylindricalShape * _beamPipeShape;
  StiPlanarShape      * _vacuumShape;
  Bool_t                _TpcRefSys;
};

#endif
