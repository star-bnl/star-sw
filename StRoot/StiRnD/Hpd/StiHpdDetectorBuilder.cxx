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
#include "StiHpdDetectorBuilder.h" 
#include "StiHpdIsActiveFunctor.h"
#include "StGetConfigValue.hh"
#include "Sti/StiElossCalculator.h"
#include "TMath.h"

StiHpdDetectorBuilder::StiHpdDetectorBuilder(bool active, const string & inputFile)
    : StiDetectorBuilder("Hpd",active,inputFile)
{
    //Parameterized hit error calculator.  Given a track (dip, cross, pt, etc) returns average error
    //once you actually want to do tracking, the results depend strongly on the numbers below.
    //here I plug in 10.micron resolution in both local x and y coordinates
    //I also put no dependence on either crossing angle or dip angle of track
    _trackingParameters.setName("HpdTrackingParameters");
    _calculator.setName("hpdHitError");
    // _calculator = new StiDefaultHitErrorCalculator();
    //_calculator.set(1, 0., 0., 1., 0., 0.);
    //StiTrackingParameters * trackingPars = getTrackingParameters();


    ifstream inF("HpdBuilder_pars.txt");
    if (inF)
      {
	_trackingParameters.loadFS(inF);
	cout << "StiHpdDetectorBuilder:: -I-  New tracking parameters from file" << endl;
      }
    else
      {
	_trackingParameters.setMaxChi2ForSelection(10.);
	_trackingParameters.setMinSearchWindow(1.);
	_trackingParameters.setMaxSearchWindow(5.);
	_trackingParameters.setSearchWindowScaling(4.);
	cout << "Loading default parameters" << endl;
      }
}

StiHpdDetectorBuilder::~StiHpdDetectorBuilder()
{}

/// Build all detector components of the Hpd detector.
void StiHpdDetectorBuilder::buildDetectors(StMaker&source)
{
    char name[50];
    cout << "StiHpdDetectorBuilder::buildDetectors() -I- Started" << endl;
    load(_inputFile, source);
    const unsigned int nRows=1;
    setNRows(nRows);
    int nsectors[1];
    //   const char* detectorParamFile = "/star/u/salur/hpd/HpdGeomParams.txt";
    //  cout <<"get input values from file:\t"<<detectorParamFile<<endl;
    //StGetConfigValue(detectorParamFile,"nLadder",nsectors[0]);
    cout<<"Name of our detector"<<getGroupId()<<endl;
    setNSectors(0,48);
    cout <<"create gasses"<<endl;
    //_gas is the gas that the hpd detector lives in
    _gas            = add(new StiMaterial("HpdAir",    7.3,  14.61, 0.001205, 30420.*0.001205, 7.3*12.e-9) );
    //_fcMaterial is the (average) material that makes up the detector elements.  Here I use ~silicon
    _fcMaterial     = add(new StiMaterial("HpdSi", 14.,      28.0855,   2.33,     21.82,           14.*12.*1e-9) );

    double ionization=_fcMaterial->getIonization();
    StiElossCalculator* fcMatElossCalculator=new StiElossCalculator(_fcMaterial->getZOverA(),ionization*ionization,_fcMaterial->getA(),_fcMaterial->getZ(),_fcMaterial->getDensity());

    double radii[nRows];
    radii[0]=9.1;

    //    StGetConfigValue(detectorParamFile,"radius",radii[0]);
    double dphi[nRows];

    for(size_t zzz=0;zzz<nRows;zzz++){
      dphi[zzz]=2.*TMath::Pi()/getNSectors(zzz);
    double tiltAngle;
    tiltAngle =30;    
    tiltAngle=TMath::Pi()*(tiltAngle)/180.;
    
    double depth[nRows];
    depth[0]=49.5;
    double shiftangle=3.75;
    shiftangle=TMath::Pi()*(shiftangle)/180.;
     
    cout <<"begin loop on rows to build detectors"<<endl;
    StiPlanarShape *pShape;
    for (unsigned int row=0; row<nRows; row++)
	{
	  pShape = new StiPlanarShape;
	  if (!pShape) throw runtime_error("StiHpdDetectorBuilder::buildDetectors() - FATAL - pShape==0||ifcShape==0");
	  sprintf(name, "Hpd/Layer_%d", row);
	  pShape->setName(name);
	  double Thickness=0.178;
	  //	  StGetConfigValue(detectorParamFile,"ladder thickness",Thickness);
	  pShape->setThickness(Thickness); //cm
	    pShape->setHalfDepth( depth[0]/2. ); //extent in z
	    double sWidth=1.28;//1.68;
	    //StGetConfigValue(detectorParamFile,"sensor width",sWidth);
	    pShape->setHalfWidth(sWidth/2.); //length or a plane
	    for(unsigned int sector = 0; sector<getNSectors(row); sector++)	
	      {      
		StiPlacement *pPlacement = new StiPlacement;
		pPlacement->setZcenter(0.);
		
		double anglepos=static_cast<double>(sector)*dphi[row]+shiftangle;
		pPlacement->setNormalRep(anglepos-tiltAngle,
					 radii[row]*cos(tiltAngle),
					 radii[row]*sin(tiltAngle)) ;
		pPlacement->setLayerRadius(radii[row]);
		
		pPlacement->setRegion(StiPlacement::kMidRapidity);
		pPlacement->setLayerAngle(anglepos);
		
		sprintf(name, "Hpd/Layer_%d/Ladder_%d", row, sector);
		cout<<"\tbuild detector with name:\t "<<name<<endl;
		StiDetector *pDetector = _detectorFactory->getInstance();
		pDetector->setName(name);
		pDetector->setIsOn(true);
		pDetector->setIsActive(new StiHpdIsActiveFunctor);
		pDetector->setIsContinuousMedium(true);
		pDetector->setIsDiscreteScatterer(true);
		pDetector->setMaterial(_fcMaterial);
		pDetector->setGas(_gas);
		pDetector->setShape(pShape);
		pDetector->setPlacement(pPlacement);
	        cout<<"HPD value of the normal radius:from Placement "<<pPlacement->getNormalRadius()<<endl;
		cout<<"Pos Angle: "<<anglepos<<"TiltAngle: "<<tiltAngle<<"Radii: "<<radii[row]<<endl;		
		cout<<"HPD value of the normal radius: "<<pDetector->getPlacement()->getNormalRadius()<<endl;
		pDetector->setHitErrorCalculator(&_calculator);
		pDetector->setElossCalculator(fcMatElossCalculator);
		pDetector->setKey(1,row);
		pDetector->setKey(2,sector);
		
		//Put in container
		add(row, sector, pDetector);
	      }
	}
    }
    cout << "StiHpdDetectorBuilder::buildDetectors() -I- Done" << endl;
}


void StiHpdDetectorBuilder::loadDS(TDataSet& ds)
{
	cout << "StiHpdDetectorBuilder::loadDS(TDataSet* ds) -I- Started" << endl;
	//	_trackingParameters.loadDS(ds);
	_calculator.loadDS(ds);
	cout << "StiHpdDetectorBuilder::loadDS(TDataSet* ds) -I- Done" << endl;
}
