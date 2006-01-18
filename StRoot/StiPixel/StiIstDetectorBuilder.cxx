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
#include "StiIstDetectorBuilder.h" 
#include "StiIstIsActiveFunctor.h"
#include "StGetConfigValue.hh"

StiIstDetectorBuilder::StiIstDetectorBuilder(bool active, const string & inputFile)
    : StiDetectorBuilder("Ist",active,inputFile)
{
    //Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
    //once you actually want to do tracking, the results depend strongly on the numbers below.
    //here I plug in 10.micron resolution in both local x and y coordinates
    //I also put no dependence on either crossing angle or dip angle of track
    _trackingParameters.setName("IstTrackingParameters");
    _calculator.setName("IstHitErrors");
    //_calculator = new StiDefaultHitErrorCalculator();
    _calculator.set(1.e-4, 0., 0., 1.e-4, 0., 0.);
    //StiTrackingParameters * trackingPars = getTrackingParameters();


    ifstream inF("IstBuilder_pars.txt");
    if (inF)
      {
	_trackingParameters.loadFS(inF);
	cout << "StiIstDetectorBuilder:: -I-  New tracking parameters from file" << endl;
      }
    else
      {
	_trackingParameters.setMaxChi2ForSelection(30.);
	_trackingParameters.setMinSearchWindow(0.2);
	_trackingParameters.setMaxSearchWindow(0.8);
	_trackingParameters.setSearchWindowScaling(10.);
	cout << "Loading default parameters" << endl;
      }
}

StiIstDetectorBuilder::~StiIstDetectorBuilder()
{}

/// Build all detector components of the Ist detector.
void StiIstDetectorBuilder::buildDetectors(StMaker&source)
{
    char name[50];
    cout << "StiIstDetectorBuilder::buildDetectors() -I- Started" << endl;
    const unsigned int nRows=3;
    setNRows(nRows);
    int nsectors[3];
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","nLadder1",nsectors[0]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","nLadder2",nsectors[1]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","nLadder3",nsectors[2]);
    setNSectors(0,nsectors[0]);
    setNSectors(1,nsectors[1]);
    setNSectors(2,nsectors[2]);
    //_gas is the gas that the ist detector lives in
    _gas            = add(new StiMaterial("IstAir",     0.49919,  1., 0.001205, 30420.*0.001205, 5.) );
    //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
    _fcMaterial     = add(new StiMaterial("IstSi", 14.,      28.0855,   2.33,     21.82,           5.) );

    double radii[nRows];
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","r1",radii[0]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","r2",radii[1]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","r3",radii[2]);
    double dphi[nRows];
    for(int zzz=0;zzz<nRows;zzz++)
      dphi[zzz]=2.*M_PI/nsectors[zzz];
    double tiltAngle;
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","aOffset",tiltAngle);
    tiltAngle=M_PI*(90.-tiltAngle)/180.;
    double perOffset;
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","pPerOffset",perOffset);
    double parOffset;
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","pParOffset",parOffset);
    double depth[nRows];
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","length1",depth[0]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","length2",depth[1]);
    StGetConfigValue("/star/u/wleight/fromMike/params.txt","length3",depth[2]);

    StiPlanarShape *pShape;
    for (unsigned int row=0; row<nRows; row++)
	{
	    pShape = new StiPlanarShape;
	    if (!pShape) throw runtime_error("StiIstDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");
	    sprintf(name, "Ist/Layer_%d", row);
	    pShape->setName(name);
	    double Thickness;
	    StGetConfigValue("/star/u/wleight/fromMike/params.txt","ladder thickness",Thickness);
	    pShape->setThickness(Thickness); //cm
	    pShape->setHalfDepth( depth[row]/2. ); //extent in z
	    double sWidth;
	    StGetConfigValue("/star/u/wleight/fromMike/params.txt","sensor width",sWidth);
	    pShape->setHalfWidth(sWidth/2.); //length or a plane
	    for(unsigned int sector = 0; sector<getNSectors(row); sector++)	
		{      
		    StiPlacement *pPlacement = new StiPlacement;
		    pPlacement->setZcenter(0.);

		    //double anglepos = static_cast<double>(sector+1)*dphi[row]-atan(parOffset/(radii[row]+perOffset));
		    double anglepos=static_cast<double>(sector+1)*dphi[row];
		    //double rlad = radii[row]+perOffset;
		    //double tilt = atan(1.5/(radii[row]+.238))+tiltAngle[row];

		    //double psi = phi + tilt;
		    //double rtrue = sqrt(rlad*rlad + parOffset*parOffset);

		    pPlacement->setCenterRep(anglepos,radii[row],tiltAngle); 
		    pPlacement->setLayerRadius(radii[row]);
		    pPlacement->setRegion(StiPlacement::kMidRapidity);
		    sprintf(name, "Ist/Layer_%d/Ladder_%d", row, sector);
		    //cout<<"NAME: "<<name<<endl;
		    StiDetector *pDetector = _detectorFactory->getInstance();
		    pDetector->setName(name);
		    pDetector->setIsOn(true);
		    pDetector->setIsActive(new StiIstIsActiveFunctor);
		    pDetector->setIsContinuousMedium(true);
		    pDetector->setIsDiscreteScatterer(true);
		    pDetector->setMaterial(_gas);
		    pDetector->setGas(_gas);
		    pDetector->setShape(pShape);
		    pDetector->setPlacement(pPlacement);
		    pDetector->setHitErrorCalculator(&_calculator);

		    //Put in container
		    add(row, sector, pDetector);
		}
	}
    cout << "StiIstDetectorBuilder::buildDetectors() -I- Done" << endl;
}



