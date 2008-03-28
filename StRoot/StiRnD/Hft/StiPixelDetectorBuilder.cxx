/*
 * $Id: StiPixelDetectorBuilder.cxx,v 1.12 2006/02/23 00:22:54 andrewar Exp $
 *
 * $Log: StiPixelDetectorBuilder.cxx,v $
 * Revision 1.12  2006/02/23 00:22:54  andrewar
 * Set Detector Id to kHftId, corrected Ist*pars -> Pixel*pars
 *
 * Revision 1.11  2006/02/17 21:39:32  andrewar
 * Added calls to StiDetector::setKey(key,val)
 *
 *
 */

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
#include "Sti/StiElossCalculator.h"
#include "StiPixelDetectorBuilder.h" 
#include "StiPixelIsActiveFunctor.h"

#include "StEvent.h"
#include "StEventTypes.h"

StiPixelDetectorBuilder::StiPixelDetectorBuilder(bool active,
						 const string & inputFile)
  : StiDetectorBuilder("Pixel",active,inputFile)
{
	//Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
	//once you actually want to do tracking, the results depend strongly on the numbers below.
	//here I plug in 4micron resolution in both local x and y coordinates
	//I also put no dependence on either crossing angle or dip angle of track
    _trackingParameters.setName("PixelTrackingParameters");
    _calculator.setName("PixelHitErrors");

    //_calculator = new StiDefaultHitErrorCalculator();
    _calculator.set(6e-5, 0., 0., 6e-5, 0., 0.);
  
    ifstream inF("PixelBuilder_pars.txt");
    if (inF)
      {
	_trackingParameters.loadFS(inF);
	cout << "StiPixelDetectorBuilder:: -I-  New tracking parameters from file" << endl;
      }
    else
      {

	//StiTrackingParameters * trackingPars = getTrackingParameters();
	_trackingParameters.setMaxChi2ForSelection(100.);
	_trackingParameters.setMinSearchWindow(0.01);
	_trackingParameters.setMaxSearchWindow(.5);
	_trackingParameters.setSearchWindowScaling(10.);
      }
}

StiPixelDetectorBuilder::~StiPixelDetectorBuilder()
{}

/// Build all detector components of the Pixel detector.
void StiPixelDetectorBuilder::buildDetectors(StMaker&source)
{
  char name[50];
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Started" << endl;
  unsigned int nRows=1;
  setNRows(2);
  setNSectors(0,6);
  setNSectors(1,18);
  
  //_gas is the gas that the pixel detector lives in
  _gas            = add(new StiMaterial("PixelAir",7.3, 14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9));
  //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
  StiMaterial * material = add(new StiMaterial("PixelSi", 14.,  28.0855,   2.33,     21.82,   14.*12.*1e-9) );


  //Instantiate energy loss detector for si material  
  //const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  double ionization = material->getIonization();
  StiElossCalculator * siElossCalculator = new StiElossCalculator(material->getZOverA(),
								  ionization*ionization,
								  material->getA(),
								  material->getZ(),
								  material->getDensity());
  
  StiPlanarShape *pShape;
  for (unsigned int row=0; row<nRows; row++) 
    {
      pShape = new StiPlanarShape;
      if (!pShape) throw runtime_error("StiPixelDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");
      sprintf(name, "Pixel/Layer_%d", row);
      pShape->setName(name);
      pShape->setThickness(0.0280); //cm 
      pShape->setHalfDepth( 20./2. );
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
	  pDetector->setMaterial(material);
	  pDetector->setGas(_gas);
	  pDetector->setGroupId(kHftId);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  pDetector->setHitErrorCalculator(&_calculator);
	  pDetector->setElossCalculator(siElossCalculator);
	  if (sector<18)
	    {
	      pDetector->setKey(1,1);
	      pDetector->setKey(2,sector);
	      add(1,sector,pDetector);
	    }
	  else
	    {
	      pDetector->setKey(1,0);
	      pDetector->setKey(2,sector-18);
	      add(0,(sector-18),pDetector);
	    }
	}
    }
  cout << "StiPixelDetectorBuilder::buildDetectors() -I- Done" << endl;
}



