#include <stdio.h>
#include "Stiostream.h"
#include <stdexcept>
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcPadPlaneI.h"
#include "StTpcDb/StTpcDimensionsI.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiDetector.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiIsActiveFunctor.h"
#include "Rtypes.h"
#include "Stiostream.h"
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiHitErrorCalculator.h"
#include "StiTpcDetectorBuilder.h" 
#include "StiTpcIsActiveFunctor.h"
#include "Sti/StiElossCalculator.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
#include "tables/St_HitError_Table.h"
#include "StMatrixD.hh"

StiTpcDetectorBuilder::StiTpcDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Tpc",active,inputFile)
{
  _trackingParameters.setName("tpcTrackingParameters");
  _innerCalc.setName("tpcInnerHitError");
  _outerCalc.setName("tpcOuterHitError");
}



StiTpcDetectorBuilder::~StiTpcDetectorBuilder()
{}


/*! Build all detector components of the TPC.
The material currently used are P10, and NOMEX. The properties
of these materials are extracted from the Particle Data Book.
The detector components of the TPC include the 24 sectors, 45 padrow gas volumes, and
the inner and outer field cage of the TPC. The padrows  are polygonal with 12  sides
whereas  the field cage are cylindrical. However to match the 12 fold symmetry of the 
TPC, the field cage are artificially segmented into 12 sectors each.
*/
void StiTpcDetectorBuilder::buildDetectors(StMaker&source)
{
  char name[50];
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Started" << endl;
  unsigned int row;
  load(_inputFile,source);
  
  if (!gStTpcDb) 
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- gStTpcDb==0");
  
  _padPlane = gStTpcDb->PadPlaneGeometry();
  if (!_padPlane) 
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- _padPlane==0");
  
  _dimensions = gStTpcDb->Dimensions();
  if (!_dimensions)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- _dimensions==0");
  
  // change to +1 instead of +2 to remove the ofc.
  unsigned int nRows = _padPlane->numberOfRows()+1;
  setNRows(nRows);
  for(unsigned int row = 0; row<nRows;row++)
    {
      setNSectors(row,12);
    }

  //Coordinate transform used to do alignment of pad rows...
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  local2GlobalRotation; 
  StMatrixD  unit(3,3,1);
  StThreeVectorD RowPosition;

  _gas        = add(new StiMaterial("P10",   16.4,  36.2741, 0.00156,  12820.*0.00156, 15.8*18.*1e-9) ); 
  _fcMaterial = add(new StiMaterial("Nomex",  6.24, 12.40,   0.064,       39.984,      6.24*12.*1e-9) );

  
  const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  // Instantiate eloss calculator for tpc gas and for field cage
  double ionization = _gas->getIonization();
  StiElossCalculator * gasElossCalculator = new StiElossCalculator(_gas->getZOverA(),ionization*ionization);
  ionization = _fcMaterial->getIonization();
  StiElossCalculator * fcElossCalculator = new StiElossCalculator(_fcMaterial->getZOverA(), ionization*ionization);


  // Inner field cage
  StiCylindricalShape *ifcShape = new StiCylindricalShape;
  ifcShape->setName("tpc/ifc");
  ifcShape->setThickness(1.27); 
  ifcShape->setHalfDepth( _dimensions->tpcTotalLength()/2. );
  ifcShape->setOpeningAngle( M_PI/6. );
  ifcShape->setOuterRadius(_dimensions->ifcRadius() + ifcShape->getThickness()/2.);
  if (!ifcShape)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() - FATAL - ifcShape==0");
  float fIfcRadius = _dimensions->ifcRadius();   

  /*
  //Outer  field cage
  StiCylindricalShape *ofcShape = new StiCylindricalShape;
  ofcShape->setName("tpc/ofc");
  ofcShape->setThickness(1.27); 
  ofcShape->setHalfDepth( _dimensions->tpcTotalLength()/2. );
  ofcShape->setOpeningAngle( M_PI/6. );
  ofcShape->setOuterRadius(_dimensions->ofcRadius() + ofcShape->getThickness()/2.);
  if (!ofcShape)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() - FATAL - ofcShape==0");
  float fOfcRadius = _dimensions->ofcRadius();   
  */

  StiPlacement *p;
  //for(unsigned int sector = 0; sector<getNSectors(); sector++) 
  for(unsigned int sector = 0; sector<12; sector++)
    {
      // inner field cage
      p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(fIfcRadius);
      p->setLayerAngle(phiForTpcSector(sector));
      p->setNormalRep(phiForTpcSector(sector), fIfcRadius, 0.);
      p->setRegion(StiPlacement::kMidRapidity);
      
      StiDetector *ifcVolume = _detectorFactory->getInstance();
      sprintf(name, "TPC/Ifc/Sector_%d", sector);
      ifcVolume->setName(name);
      ifcVolume->setIsOn(false);
      ifcVolume->setIsActive(new StiNeverActiveFunctor);
      ifcVolume->setIsContinuousMedium(false);
      ifcVolume->setIsDiscreteScatterer(true);
      ifcVolume->setShape(ifcShape);
      ifcVolume->setPlacement(p);
      ifcVolume->setGas(_gas);
      ifcVolume->setMaterial(_fcMaterial);
      ifcVolume->setElossCalculator(fcElossCalculator);
      //add(ifcVolume);
      add(45,sector,ifcVolume);

      /*
      // outer field cage
      p = new StiPlacement;
      p->setZcenter(0.);
      p->setLayerRadius(fOfcRadius);
      p->setLayerAngle(phiForTpcSector(sector));
	  
      p->setNormalRep(phiForTpcSector(sector), fOfcRadius, 0.);
      p->setRegion(StiPlacement::kMidRapidity);      

      
      StiDetector *ofcVolume = _detectorFactory->getInstance();
      sprintf(name, "TPC/Ofc/Sector_%d", sector);
      ofcVolume->setName(name);
      ofcVolume->setIsOn(false);
      ofcVolume->setIsActive(new StiNeverActiveFunctor);
      ofcVolume->setIsContinuousMedium(false);
      ofcVolume->setIsDiscreteScatterer(true);
      ofcVolume->setShape(ofcShape);
      ofcVolume->setPlacement(p);
      ofcVolume->setGas(_gas);
      ofcVolume->setMaterial(_fcMaterial);
      ofcVolume->setElossCalculator(fcElossCalculator);
      // remove it for now...
      //add(ofcVolume);  
      add(46,sector,ofcVolume);
      */
  } 

  int debug = 0;

  StDetectorDbTpcRDOMasks *s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  StiPlanarShape *pShape;
  //Active TPC padrows 
  double radToDeg = 180./3.1415927;
  unsigned int _nInnerPadrows = _padPlane->numberOfInnerRows();
  for(row = 0; row<45; row++)
    {
      //Nominal pad row information.
      // create properties shared by all sectors in this padrow
      float fRadius = _padPlane->radialDistanceAtRow(row+1);
      sprintf(name, "Tpc/Padrow_%d", row);
      pShape = new StiPlanarShape;
      if (!pShape)
	throw runtime_error("StiTpcDetectorBuilder::buildDetectors() - FATAL - pShape==0||ofcShape==0");
      if(row<_nInnerPadrows)
	pShape->setThickness(_padPlane->innerSectorPadLength());
      else
	pShape->setThickness(_padPlane->outerSectorPadLength());
      pShape->setHalfDepth(_dimensions->tpcTotalLength()/2.);
      pShape->setHalfWidth(_padPlane->PadPitchAtRow(row+1) * _padPlane->numberOfPadsAtRow(row+1) / 2.);
      pShape->setName(name);
      for(unsigned int sector = 0; sector<getNSectors(); sector++)
	{


	  //Retrieve position and orientation of the TPC pad rows from the database.
	  StTpcLocalSectorDirection  dirLS[3];
	  dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,sector+1,row+1);
	  dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,sector+1,row+1);
	  dirLS[2] = StTpcLocalSectorDirection(0.,0.,1.,sector+1,row+1);
	  local2GlobalRotation = unit;
	  for (int i = 0; i < 3; i++) 
	    {
	      if (debug>1) cout << "dirLS\t" << dirLS[i] << endl;
	      StTpcLocalDirection        dirL;      
	      StTpcLocalSectorAlignedDirection  dirLSA;
	      transform(dirLS[i],dirLSA);   if (debug>1) cout << "dirLSA\t" << dirLSA << endl;
	      transform(dirLSA,dirL);       if (debug>1) cout << "dirL\t" << dirL << endl;
	      StGlobalDirection          dirG;
	      transform(dirL,dirG);      if (debug>1) cout << "dirG\t" << dirG << endl;
	      local2GlobalRotation(i+1,1) = dirG.position().x();
	      local2GlobalRotation(i+1,2) = dirG.position().y();
	      local2GlobalRotation(i+1,3) = dirG.position().z();
	    }
	  if (debug>1) cout << "Local2GlobalRotation = " << local2GlobalRotation << endl;
	  double y  = transform.yFromRow(row+1);
	  StTpcLocalSectorCoordinate  lsCoord(0., y, 0, sector+1, row+1); if (debug>1) cout << lsCoord << endl;
	  StTpcLocalSectorAlignedCoordinate lsCoordA;  
	  transform(lsCoord,lsCoordA);                       if (debug>1) cout << lsCoordA << endl;                   
	  StGlobalCoordinate  gCoord; 
	  transform(lsCoordA, gCoord);                       if (debug>1) cout << gCoord << endl;                   

	  //unit vector normal to the pad plane
	  double nx = local2GlobalRotation(2,1);
	  double ny = local2GlobalRotation(2,2);
	  double nz = local2GlobalRotation(2,3);
	  double nt = sqrt(nx*nx+ny*ny);
	  double xc = gCoord.position().x();
	  double yc = gCoord.position().y();
	  double zc = gCoord.position().z();
	  double rc = sqrt(xc*xc+yc*yc);
	  double rn = xc*nx/nt + yc*ny/nt;
	  double phic = atan2(yc,xc);
	  double phi2 = asin((xc*ny-nx*yc)/(nt*rc));
	  if (debug) cout << "row:"<<row<<" sector:"<<sector<<" fRadius:"<<fRadius<<" rn:"<<rn<<" rc:"<<rc<<" phi(nominal):"<<phiForTpcSector(sector)*radToDeg<<" phic:"<<phic*radToDeg<<" phi2:"<< phi2*radToDeg<<endl;
	  // create unique detector properties (placement & name)
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);
	  pPlacement->setCenterRep(phic, rn, phi2); 

	  pPlacement->setLayerRadius(fRadius);
	  pPlacement->setLayerAngle(phiForTpcSector(sector));

	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  sprintf(name, "Tpc/Padrow_%d/Sector_%d", row, sector);
	  // fill in the detector object and save it in our vector
	  StiDetector *pDetector = _detectorFactory->getInstance();
	  pDetector->setName(name);
	  pDetector->setIsOn(true);

	  int iRdo = rdoForPadrow(row+1);
	  bool west = s_pRdoMasks->isOn(sector+1, iRdo);
	  bool east = s_pRdoMasks->isOn( 24-(sector+1)%12, iRdo);
	  // temp overide...
	  //if (row==12 || 
	  //(sector==3 && (row==13 || row==14 || row==15 || row==16 || row==17 || row==18 || row==19 || row==20)))
	  if (row==12)
	    {
	      east = false;
	      west = false;
	    }
	  pDetector->setIsActive(new StiTpcIsActiveFunctor(_active,west,east));
	  pDetector->setIsContinuousMedium(true);
	  pDetector->setIsDiscreteScatterer(false);
	  pDetector->setMaterial(_gas);
	  pDetector->setGas(_gas);
	  pDetector->setShape(pShape);
	  pDetector->setPlacement(pPlacement);
	  if (row<13)
	    pDetector->setHitErrorCalculator(&_innerCalc);
	  else
	    pDetector->setHitErrorCalculator(&_outerCalc);
	  pDetector->setElossCalculator(gasElossCalculator);
	  add(row,sector,pDetector);
	}// for sector
    }// for row

  
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Done" << endl;
}



void StiTpcDetectorBuilder::loadDS(TDataSet &ds)
{
	cout << "StiTpcDetectorBuilder::load(TDataSet * ds) -I- Loading TPC tracking parameters from tracking database" << endl;
	getTrackingParameters().loadDS(ds);
	_innerCalc.loadDS(ds);
	_outerCalc.loadDS(ds);
	cout << "StiTpcDetectorBuilder::load(TDataSet * ds) -I- Loading TPC tracking parameters from tracking database" << endl;
}

void StiTpcDetectorBuilder::loadFS(ifstream & inputFileStream)
{
	cout <<"StiTpcDetectorBuilder::load(ifstream &) -I- Loading TPC tracking parameters from file" << endl;
	getTrackingParameters().loadFS(inputFileStream);
	_innerCalc.loadFS(inputFileStream);
	_outerCalc.loadFS(inputFileStream);
	cout <<"StiTpcDetectorBuilder::load(ifstream &) -I- Done loading TPC tracking parameters from file" << endl;
}

void StiTpcDetectorBuilder::setDefaults()
{
  cout <<"StiTpcDetectorBuilder::setDefaults() -I- Tracking Parameters set from class default values."<<endl;
  _trackingParameters.setMaxChi2ForSelection(10.);
  _trackingParameters.setMinSearchWindow(1.6);
  _trackingParameters.setMaxSearchWindow(7.);
  _trackingParameters.setSearchWindowScaling(15.);
  _innerCalc.set(.066, 1.2e-04, 0.0004, 0.066, 4.4e-4, 2.8e-02);
  _outerCalc.set(.02, 4.e-3, 0.04, 0.02, 3.2e-3, 9.e-2);
  cout << _trackingParameters << endl;
  cout << _innerCalc << endl;
  cout << _outerCalc << endl;
  cout <<"StiTpcDetectorBuilder::setDefaults() -I- Tracking Parameters set from class default values."<<endl;
}


/*







 */
