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
  //One row
  setNRows(1);
  //12 Sectors
  setNSectors(0,12);

 _pipeMaterial = add(new StiMaterial("Be", 4.,9.012, 1.848, 65.19, 3.) );  
  _vacuumMaterial = add(new StiMaterial("Vaccum",0., 1., 0., 1e30, 0.)  );
  _beamPipeShape = new StiCylindricalShape;
  _beamPipeShape->setName("Star/pipe");
  _beamPipeShape->setThickness(0.1); // to be checked
  _beamPipeShape->setHalfDepth( 200.);
  _beamPipeShape->setOpeningAngle( M_PI/6. );
  _beamPipeShape->setOuterRadius(4.00);// to be checked
  add(_beamPipeShape);

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
      add(0,sector,pipeVolume);
    }
 cout << "StiStarDetectorBuilder::buildDetectors() -I- Done" << endl;
}


