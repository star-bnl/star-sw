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

StiStarDetectorBuilder::StiStarDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("StarBuilder",active,inputFile)
{}

StiStarDetectorBuilder::~StiStarDetectorBuilder()
{}

void StiStarDetectorBuilder::buildDetectors(StMaker&s)
{
	string name;
  cout << "StiStarDetectorBuilder::buildDetectors() -I- Started" << endl;
  setNRows(1);
  setNSectors(0,12);
	float radius = 4.;  // Kai - set this to 1.45
	float thickness = 0.076;
  float dPhi=M_PI/6.;
	float depth = 20.;
  _pipeMaterial = add(new StiMaterial("Be", 4.,9.012, 1.848, 65.19, 3.) );  
  _vacuumMaterial = add(new StiMaterial("Vaccum",0., 1., 0., 1e30, 0.)  );
  _beamPipeShape = new StiCylindricalShape;
  _beamPipeShape->setName("Star/pipe");
  _beamPipeShape->setThickness(thickness); // checked
  _beamPipeShape->setHalfDepth( depth );
  _beamPipeShape->setOpeningAngle( dPhi );
  _beamPipeShape->setOuterRadius(radius+thickness/2.);// checked
  add(_beamPipeShape);
  for(unsigned int sector = 0; sector<12; sector++)
    {
      StiPlacement *p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(radius);
      p->setNormalRep(sector*dPhi, radius, 0.);
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
      add(0,sector,pipeVolume);
    }
 cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}


