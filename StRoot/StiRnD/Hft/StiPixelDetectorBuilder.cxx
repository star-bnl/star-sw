//std
#include <stdio.h>
#include <stdexcept>

//Sti Base includes
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/Base/Messenger.h"
#include "Sti/StiHitErrorCalculator.h"

//StiPixel includes
#include "StiPixelDetectorBuilder.h" 
#include "StiPixelIsActiveFunctor.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active)
  : StiDetectorBuilder("PixelBuilder",active)
{
    //Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
    //once you actually want to do tracking, the results depend strongly on the numbers below.
    //here I plug in 4micron resolution in both local x and y coordinates
    //I also put no dependence on either crossing angle or dip angle of track
    _innerCalc = new StiDefaultHitErrorCalculator();
    _innerCalc->set(0.00004, 0., 0., 0.00004, 0., 0.);
}

StiPixelDetectorBuilder::~StiPixelDetectorBuilder()
{
  delete _innerCalc;
}

/*! Builds the material required for the Pixel
<p>
The material currently used are P10, and NOMEX. The properties
of these materials are extracted from the Particle Data Book.
*/
void StiPixelDetectorBuilder::buildMaterials()
{
    _messenger << "StiPixelDetectorBuilder::buildMaterials() - INFO - Started" << endl;
    
    //_gas is the gas that the pixel detector lives in
    _gas            = add(new StiMaterial("PixelAir",     0.49919,  1., 0.001205, 30420.*0.001205, 5.) );
    //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
    _fcMaterial     = add(new StiMaterial("PixelSi", 14.,      28.0855,   2.33,     21.82,           5.) );
    
    _messenger << "StiPixelDetectorBuilder::buildMaterials() - INFO - Done" << endl;
}

void StiPixelDetectorBuilder::buildShapes()
{
  _messenger << "StiPixelDetectorBuilder::buildShapes() - INFO - Started" << endl;

  //this is assuming that we have the same beampipe (at r=4cm).
  //To change, edit StRoot/Sti/Star/StiStarDetectorBuilder.cxx
  double pixRadius = 5.0; //cm
  
  //change for KS from IFC to Pixel
  StiCylindricalShape *ifcShape = new StiCylindricalShape;
  ifcShape->setName("Pixel/sector");
  ifcShape->setThickness(0.1); //radial thickness in cm 
  ifcShape->setHalfDepth( 16./2. ); //half depth in global z (for TPC = 400/2)
  
  //the angle swept out.  So, for 12 segments, = pi/6
  ifcShape->setOpeningAngle( M_PI/6. );
  ifcShape->setOuterRadius(pixRadius + ifcShape->getThickness()/2.); //we'll put it at 5cm
  add(ifcShape);
  
  _messenger << "StiPixelDetectorBuilder::buildShapes() - INFO - Done" << endl;
}

/*! Build all detector components of the Pixel.
<p>
The detector components of the TPC include the 24 sectors, 45 padrow gas volumes, and
the inner and outer field cage of the TPC. The padrows  are polygonal with 12  sides
whereas  the field cage are cylindrical. However to match the 12 fold symmetry of the
TPC, the field cage are artificially segmented into 12 sectors each.
*/
void StiPixelDetectorBuilder::buildDetectors()
{
  char name[50];
  _messenger << "StiPixelDetectorBuilder::buildDetectors() - INFO - Started" << endl;

  StiShape *ifcShape = findShape("Pixel/sector");  
  
  if (!ifcShape)
      throw runtime_error("StiPixelDetectorBuilder::buildDetectors() - FATAL - ifcShape==0");
  
  double pixRadius = 5.0;
  unsigned int nRows=1;
  
  StiPlacement *p;
  for (unsigned int row=0; row<nRows; ++row) {
      for(unsigned int sector = 0; sector<12; ++sector)	{
	  
	  // inner field cage changed to pixel
	  p = new StiPlacement; //see StRoot/Sti/StiPlacement.h
	  p->setZcenter(0.);
	  p->setLayerRadius(pixRadius);
	  p->setRegion(StiPlacement::kMidRapidity);
	  p->setNormalRep(phiForTpcSector(sector), pixRadius, 0.); //all stay the same except fIfcRadius
	  
	  StiDetector *ifcVolume = _detectorFactory->getInstance();
	  sprintf(name, "Pixel/Layer1/Sector_%d", sector);
	  ifcVolume->setName(name);
	  ifcVolume->setIsOn(true);
	  //ifcVolume->setIsActive(new StiIsNeverActiveFunctor); //changed this to never active (ask MLM)
	  ifcVolume->setIsActive(new StiPixelIsActiveFunctor); //changed this to always active (ask MLM)
	  ifcVolume->setIsContinuousMedium(false);
	  ifcVolume->setIsDiscreteScatterer(true);
	  ifcVolume->setShape(ifcShape);
	  ifcVolume->setPlacement(p);
	  ifcVolume->setGas(_gas);
	  ifcVolume->setMaterial(_fcMaterial);

	  //set the pointer to the hit error calculator
	  ifcVolume->setHitErrorCalculator(_innerCalc);
	  
	  add(row, sector, ifcVolume); //this hangs it in the right place!
	  
	  //test with an add and retrieve:
	  StiDetector* tdet1 = getDetector(row,sector);

	  //cout <<"building detector:\t "<<name<<endl;
	  if (tdet1!=ifcVolume) {
	      cout <<"StiPixelDetectorBuilder could not retrieve detector from self"<<endl;
	      //should stop here and abort, this should never happen!
	      abort();
	  }
	  else {
	      //cout <<"retrieved detector from self!"<<endl;
	  }
      }
  }

  _messenger << "StiPixelDetectorBuilder::buildDetectors() - INFO - Done" << endl;
}

void StiPixelDetectorBuilder::loadDb()
{
    //this has to happen in order for it to make it into event display
    setNRows(1);
    for(unsigned int row = 0; row<1; row++)
	{
	    setNSectors(row,12);
	}
}


