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
	_calculator.set(0.00004, 0., 0., 0.00004, 0., 0.);
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
	setNRows(nRows);
	for(unsigned int row = 0; row<nRows; ++row)
		setNSectors(row,12);
	cout << "StiPixelDetectorBuilder::buildMaterials() -I- Started" << endl; 
	//_gas is the gas that the pixel detector lives in
	_gas            = add(new StiMaterial("PixelAir",     0.49919,  1., 0.001205, 30420.*0.001205, 5.) );
	//_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
	_fcMaterial     = add(new StiMaterial("PixelSi", 14.,      28.0855,   2.33,     21.82,           5.) );
	cout << "StiPixelDetectorBuilder::buildMaterials() -I- Done" << endl;
  cout << "StiPixelDetectorBuilder::buildShapes() -I- Started" << endl;
  //this is assuming that we have the same beampipe (at r=4cm).
  //To change, edit StRoot/Sti/Star/StiStarDetectorBuilder.cxx
  //change for KS from IFC to Pixel
  StiCylindricalShape *shape = new StiCylindricalShape;
  shape->setName("Pixel/sector");
  shape->setThickness(0.1); //radial thickness in cm 
  shape->setHalfDepth( 16./2. ); 
  
  //the angle swept out.  So, for 12 segments, = pi/6
  shape->setOpeningAngle( M_PI/6. );
  shape->setOuterRadius(pixRadius + shape->getThickness()/2.); //we'll put it at 5cm
  add(shape);
  cout << "StiPixelDetectorBuilder::buildShapes() -I- Done" << endl;
  StiPlacement *p;
  for (unsigned int row=0; row<nRows; ++row) 
		{
			for(unsigned int sector = 0; sector<12; ++sector)	
				{
					StiDetector *detector = _detectorFactory->getInstance();
					p = new StiPlacement; //see StRoot/Sti/StiPlacement.h
					p->setZcenter(0.);
					p->setLayerRadius(pixRadius);
					p->setRegion(StiPlacement::kMidRapidity);
					p->setNormalRep(phiForSector(sector), pixRadius, 0.);
					sprintf(name, "Pixel/Layer1/Sector_%d", sector);
					detector->setName(name);
					detector->setIsOn(true);
					detector->setIsActive(new StiPixelIsActiveFunctor); 
					detector->setIsContinuousMedium(false);
					detector->setIsDiscreteScatterer(true);
					detector->setShape(shape);
					detector->setPlacement(p);
					detector->setGas(_gas);
					detector->setMaterial(_fcMaterial);
					detector->setHitErrorCalculator(&_calculator);
					add(row, sector, detector); 
				}
		}
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Done" << endl;
}



