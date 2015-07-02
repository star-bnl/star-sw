#include "StiEmc/StiEmcDetectorBuilder.h" 
#include "StiEmc/StiEmcIsActiveFunctor.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include <stdexcept>

StiEmcDetectorBuilder::StiEmcDetectorBuilder(bool active)
  : StiDetectorBuilder("EmcBuilder",active)
{}

StiEmcDetectorBuilder::~StiEmcDetectorBuilder()
{}

void StiEmcDetectorBuilder::buildDetectors(StMaker & source)
{
  string name;
  setNRows(3); //preshower, shower-max, tower
  for (unsigned int barrel = 0; barrel<3; barrel++)
    setNSectors(barrel,16);

  _gas  = add(new StiMaterial("Pb",82., 207.2, 21.45, 0.56, 82*12e-9));
  _lead = add(new StiMaterial("Pb",82., 207.2, 21.45, 0.56, 82*12e-9));
  //    ideally get the following based on some db calls.
  float halfDepth=1; // half length along beam axis
  float thickness=1; // radial thickness
  float halfWidth=1; // half length y-axis (azimuthal)
  _preShower = add(new StiPlanarShape("Emc/PreShower",halfDepth,thickness,halfWidth));
  _showerMax = add(new StiPlanarShape("Emc/ShowerMax",halfDepth,thickness,halfWidth));
  _tower = add(new StiPlanarShape("Emc/Tower",halfDepth,thickness,halfWidth));

  // loop over all EMC "sectors"
  int nSector=16;
  int iSector, iLayer=0;
  StiDetector *detector; 
  StiPlacement *place ;
  for (iSector=0;iSector<nSector;iSector++)
    {
      float radius=220; // must be set properly
      float phi=3.14*iSector/8; // must be set properly
      float zCenter=0.;//must be set properly
      // Preshower
      place = new StiPlacement;
      place->setZcenter(zCenter);
      place->setNormalRep(phi,radius, 0.); 
      place->setLayerRadius(radius);
      place->setRegion(StiPlacement::kMidRapidity);


      name = "Emc/Sector_";
      name+=iSector;
      name+="/PreShower";
      radius+=10.;
      detector = _detectorFactory->getInstance();
      detector->setName(name);
      detector->setIsOn(true);
      detector->setIsActive(new StiEmcIsActiveFunctor(iSector,iLayer));
      detector->setIsContinuousMedium(true);
      detector->setIsDiscreteScatterer(false);
      detector->setMaterial(_lead);
      detector->setGas(_gas);
      detector->setShape(_preShower);
      detector->setPlacement(place);
      add(0,iSector,detector);
      radius+=10.;
      // Showermax
      place = new StiPlacement;
      place->setZcenter(zCenter);
      place->setNormalRep(phi,radius, 0.); 
      place->setLayerRadius(radius);
      place->setRegion(StiPlacement::kMidRapidity);
      name = "Emc/Sector_";
      name+=iSector;
      name+="/ShowerMax";
      detector = _detectorFactory->getInstance();
      detector->setName(name);
      detector->setIsOn(true);
      detector->setIsActive(new StiEmcIsActiveFunctor(iSector,iLayer));
      detector->setIsContinuousMedium(true);
      detector->setIsDiscreteScatterer(false);
      detector->setMaterial(_lead);
      detector->setGas(_gas);
      detector->setShape(_preShower);
      detector->setPlacement(place);
      add(1,iSector,detector);

      // Tower
      radius+=10.;
      place = new StiPlacement;
      place->setZcenter(zCenter);
      place->setNormalRep(phi,radius, 0.); 
      place->setLayerRadius(radius);
      place->setRegion(StiPlacement::kMidRapidity);
      name = "Emc/Sector_";
      name+=iSector;
      name+="/Tower";
      detector = _detectorFactory->getInstance();
      detector->setName(name);
      detector->setIsOn(true);
      detector->setIsActive(new StiEmcIsActiveFunctor(iSector,iLayer));
      detector->setIsContinuousMedium(true);
      detector->setIsDiscreteScatterer(false);
      detector->setMaterial(_lead);
      detector->setGas(_gas);
      detector->setShape(_preShower);
      detector->setPlacement(place);
      add(2,iSector,detector);
    }
}

