#include <stdio.h>
#include <stdexcept>
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "StiPixelDetectorBuilder.h" 
#include "StiPixelIsActiveFunctor.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Pixel",active,inputFile)
{
	//Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
	//once you actually want to do tracking, the results depend strongly on the numbers below.
	//here I plug in 4micron resolution in both local x and y coordinates
	//I also put no dependence on either crossing angle or dip angle of track
  _trackingParameters.setName("PixelTrackingParameters");
  _calculator.setName("PixelHitErrors");
  //_calculator = new StiDefaultHitErrorCalculator();
  _calculator.set(4e-6, 0., 0., 4e-6, 0., 0.);
  //StiTrackingParameters * trackingPars = getTrackingParameters();
  _trackingParameters.setMaxChi2ForSelection(10000.);
  _trackingParameters.setMinSearchWindow(0.2);
  _trackingParameters.setMaxSearchWindow(2.0);
  _trackingParameters.setSearchWindowScaling(100000.);
}

StiPixelDetectorBuilder::~StiPixelDetectorBuilder()
{}

/// Build all detector components of the Pixel detector.
void StiPixelDetectorBuilder::buildDetectors(StMaker&source)
{
  char name[50];
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Started" << endl;
  double pixRadius = 5.0; //cm
  unsigned int nRows=1;
  setNRows(2);
  setNSectors(0,6);
  setNSectors(1,18);
  
  //_gas is the gas that the pixel detector lives in
  _gas            = add(new StiMaterial("PixelAir",     0.49919,  1., 0.001205, 30420.*0.001205, 5.) );
  //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
  _fcMaterial     = add(new StiMaterial("PixelSi", 14.,      28.0855,   2.33,     21.82,           5.) );
  StiPlanarShape *pShape;
  for (unsigned int row=0; row<nRows; row++) 
    {
      pShape = new StiPlanarShape;
      if (!pShape) throw runtime_error("StiPixelDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");
      sprintf(name, "Pixel/Layer_%d", row);
      pShape->setName(name);
      pShape->setThickness(0.0020); //cm 
      pShape->setHalfDepth( 16./2. );
      pShape->setHalfWidth(1.0);
      for(unsigned int sector = 0; sector<24; sector++)	
	{      
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  double phi = phiForPixelSector(sector) + psiForPixelSector(sector);
	  double r = radiusForPixelSector(sector)* cos(psiForPixelSector(sector)) - 0.0040; // note 40 microns offset
	  double dY = radiusForPixelSector(sector)*sin(psiForPixelSector(sector));
	  cout << " sector:"<<sector
	       << "    phi:"<<phi*180/3.1415
	       << " radius:"<<radiusForPixelSector(sector)
	       << " normal r:"<<r
	       << "     dY:"<<dY<<endl;
	  pPlacement->setNormalRep(phi, r, dY); 
	  pPlacement->setLayerRadius(r);
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  sprintf(name, "Pixel/Layer_%d/Ladder_%d", row, sector);
	  StiDetector *pDetector = _detectorFactory->getInstance();
	  pDetector->setName(name);
	  pDetector->setIsOn(true);
	  pDetector->setIsActive(new StiPixelIsActiveFunctor);
	  pDetector->setIsContinuousMedium(true);
	  pDetector->setIsDiscreteScatterer(false);
	  pDetector->setMaterial(_gas);
	  pDetector->setGas(_gas);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  pDetector->setHitErrorCalculator(&_calculator);
	  if (sector<18)
	    add(1,sector,pDetector);
	  else
	    add(0,(sector-18),pDetector);
	}
    }
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Done" << endl;
}



