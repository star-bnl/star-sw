#include <stdexcept>
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/Star/StiStarDetectorBuilder.h"

StiStarDetectorBuilder::StiStarDetectorBuilder(bool active)
  : StiDetectorBuilder("StarBuilder",active)
{}

StiStarDetectorBuilder::~StiStarDetectorBuilder()
{}

void StiStarDetectorBuilder::buildMaterials()
{
  _messenger << "StiStarDetectorBuilder::buildMaterials() - INFO - Started" << endl;
  _pipeMaterial = add(new StiMaterial("Be", 4.,9.012, 1.848, 65.19, 3.) );  
  _vacuumMaterial = add(new StiMaterial("Vaccum",0., 1., 0., 1e30, 0.)  );
  _messenger << "StiStarDetectorBuilder::buildMaterials() - INFO - Done" << endl;
}

void StiStarDetectorBuilder::buildShapes()
{
  _messenger << "StiStarDetectorBuilder::buildShapes() - INFO - Started" << endl;
  // beam pipe
  
  _beamPipeShape = new StiCylindricalShape;
  _beamPipeShape->setName("Star/pipe");
  _beamPipeShape->setThickness(0.1); // to be checked
  _beamPipeShape->setHalfDepth( 100.);
  _beamPipeShape->setOpeningAngle( M_PI/6. );
  _beamPipeShape->setOuterRadius(4.00);// to be checked
  add(_beamPipeShape);
  
  // vacuum
  _vacuumShape = new StiPlanarShape;
  _vacuumShape->setName("Star/vacuum");
  _vacuumShape->setThickness(1.); 
  _vacuumShape->setHalfDepth( 100.);
  _vacuumShape->setHalfWidth( 1.);
  add(_vacuumShape);
  _messenger << "StiStarDetectorBuilder::buildShapes() - INFO - Done" << endl;
}

void StiStarDetectorBuilder::buildDetectors()
{
  string name;
  _messenger << "StiStarDetectorBuilder::buildDetectors() - INFO - Started" << endl;
  // vacuum+pipe
  float dPhi=M_PI/6.;
  for(unsigned int sector = 0; sector<12; sector++)
    {
      StiPlacement *p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(4.);
      p->setNormalRep(sector*dPhi,4.,0.);
      p->setRegion(StiPlacement::kMidRapidity);

      
      StiDetector *pipeVolume = _detectorFactory->getInstance();
      name="Star/Pipe/Sector_";
      name+=sector;
      pipeVolume->setName(name);
      pipeVolume->setIsOn(true);
      pipeVolume->setIsActive(new StiNeverActiveFunctor);
      pipeVolume->setIsContinuousMedium(false);
      pipeVolume->setIsDiscreteScatterer(true);
      pipeVolume->setShape(_beamPipeShape);
      pipeVolume->setPlacement(p);
      pipeVolume->setGas(_vacuumMaterial);
      pipeVolume->setMaterial(_pipeMaterial);
      add(pipeVolume);

      /*
      p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(0.);
      p->setNormalRep(sector*dPhi,0.,0.);
      
      StiDetector *vacuumVolume = _detectorFactory->getInstance();
      name="Star/Vacuum/Sector_";
      //name+=sector;
      vacuumVolume->setName(name);
      vacuumVolume->setIsOn(true);
      vacuumVolume->setIsActive(new StiNeverActiveFunctor);
      vacuumVolume->setIsContinuousMedium(true);
      vacuumVolume->setIsDiscreteScatterer(false);
      vacuumVolume->setShape(_vacuumShape);
      vacuumVolume->setPlacement(p);
      vacuumVolume->setGas(_vacuumMaterial);
      vacuumVolume->setMaterial(_pipeMaterial);
      add(0,sector,vacuumVolume);
      */
    }
 _messenger << "StiStarDetectorBuilder::buildDetectors() - INFO - Done" << endl;
}

void StiStarDetectorBuilder::loadDb()
{
  //One row
  //setNRows(1);
  //12 Sectors
  //setNSectors(0,12);
  /*
    if (!gStStarDb) 
		throw runtime_error("StiStarDetectorBuilder::buildShapes() - ERROR - gStStarDb==0");
	_padPlane = gStStarDb->PadPlaneGeometry();
  _transform = new StStarCoordinateTransform(gStStarDb);
  if (!_transform)
    throw runtime_error("StiStarDetectorBuilder::loadDb() - ERROR - gStStarDb==0");
  */
}


