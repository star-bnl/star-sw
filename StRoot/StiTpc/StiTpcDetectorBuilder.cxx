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

//#define TPC_IDEAL_GEOM

StiTpcDetectorBuilder::StiTpcDetectorBuilder(bool active, const string & inputFile)
  : StiDetectorBuilder("Tpc",active,inputFile), _gas(0), _fcMaterial(0), _padPlane(0), _dimensions(0)
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
  if (!gStTpcDb) 
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- gStTpcDb==0");
  load(_inputFile,source);
  if (StiVMCToolKit::GetVMC()) {useVMCGeometry(); return;}
  unsigned int row;
  
  
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

  
  //  const static double I2Ar = (15.8*18) * (15.8*18) * 1e-18; // GeV**2
  // Instantiate eloss calculator for tpc gas and for field cage
  double ionization = _gas->getIonization();
  StiElossCalculator * gasElossCalculator = new StiElossCalculator(_gas->getZOverA(),ionization*ionization, _gas->getA(), _gas->getZ(), _gas->getDensity());
  ionization = _fcMaterial->getIonization();
  StiElossCalculator * fcElossCalculator = new StiElossCalculator(_fcMaterial->getZOverA(), ionization*ionization, _fcMaterial->getA(), _fcMaterial->getZ(), _fcMaterial->getDensity());


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
	  //	  double nz = local2GlobalRotation(2,3);
	  double nt = sqrt(nx*nx+ny*ny);
	  double xc = gCoord.position().x();
	  double yc = gCoord.position().y();
	  //	  double zc = gCoord.position().z();
	  double rc = sqrt(xc*xc+yc*yc);
	  double rn = xc*nx/nt + yc*ny/nt;
	  double phic = atan2(yc,xc);
	  double phi2 = asin((xc*ny-nx*yc)/(nt*rc));
	  if (debug) cout << "row:"<<row<<" sector:"<<sector<<" fRadius:"<<fRadius<<" rn:"<<rn<<" rc:"<<rc<<" phi(nominal):"<<phiForTpcSector(sector)*radToDeg<<" phic:"<<phic*radToDeg<<" phi2:"<< phi2*radToDeg<<endl;
	  // create unique detector properties (placement & name)
	  StiPlacement *pPlacement = new StiPlacement;
	  pPlacement->setZcenter(0.);

	  pPlacement->setLayerRadius(fRadius);
	  pPlacement->setLayerAngle(phiForTpcSector(sector));
	  pPlacement->setRegion(StiPlacement::kMidRapidity);
	  pPlacement->setCenterRep(phic, rn, phi2); 

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
	  pDetector->setKey(1,row);
	  pDetector->setKey(2,sector);
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
double iSti[6] = {.066      , 0.00012 , 0.0004    , 0.066     , 0.0004 , 0.028};
double oSti[6] = {.02       , 0.004   ,   0.04    , 0.02      , 0.0032 , 0.09       };

double iTpt[6] = {0.00168243, 0.005233, 0.05753410, 0.00312735, 0.015106, 0.02438060};
double oTpt[6] = {0.00020278, 0.003552, 0.06456100, 0.00815800, 0.005696, 0.04484400};

  cout <<"StiTpcDetectorBuilder::setDefaults() -I- Tracking Parameters set from class default values."<<endl;
  _trackingParameters.setMaxChi2ForSelection(10.);
  _trackingParameters.setMinSearchWindow(1.6);
  _trackingParameters.setMaxSearchWindow(7.);
  _trackingParameters.setSearchWindowScaling(15.);
//  _innerCalc.set(.066, 1.2e-04, 0.0004, 0.066, 4.4e-4, 2.8e-02);
//  _outerCalc.set(.02, 4.e-3, 0.04, 0.02, 3.2e-3, 9.e-2);
#if 0
_innerCalc.set(iSti[0], iSti[1],iSti[2], iSti[3],iSti[4], iSti[5]);
_outerCalc.set(oSti[0], oSti[1],oSti[2], oSti[3],oSti[4], oSti[5]);
#endif //0
#if 1
_innerCalc.set(iTpt[0], iTpt[1],iTpt[2], iTpt[3],iTpt[4], iTpt[5]);
_outerCalc.set(oTpt[0], oTpt[1],oTpt[2], oTpt[3],oTpt[4], oTpt[5]);
#endif //0

  cout << _trackingParameters << endl;
  cout << _innerCalc << endl;
  cout << _outerCalc << endl;
  cout <<"StiTpcDetectorBuilder::setDefaults() -I- Tracking Parameters set from class default values."<<endl;
}
//________________________________________________________________________________
void StiTpcDetectorBuilder::useVMCGeometry() {
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  const VolumeMap_t TpcVolumes[] = { 
    //  {"TPCE","the TPC system in STAR","HALL_1/CAVE_1/TPCE_1","",""},
    //  {"TPCW","the TPC supporting endcap Wheel","HALL_1/CAVE_1/TPCE_1/TPCW_1-2/*","",""},
    //  {"TPEA","one endcap placed in TPC","HALL_1/CAVE_1/TPCE_1/TPEA_1-2/*","",""},
    //  {"TPCM","the Central Membrane placed in TPC","HALL_1/CAVE_1/TPCE_1/TPCM_1","",""},
    //  {"TOFC","outer field cage - fill it with insulating gas already","HALL_1/CAVE_1/TPCE_1/TOFC_1/*","",""},
    {"TIFC","Inner Field Cage","HALL_1/CAVE_1/TPCE_1/TIFC_1","",""},
    //  {"TPGV","the Gas Volume placed in TPC","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/*","",""},
    //  {"TPSS","a division of gas volume corresponding to a supersectors","HALL_1/CAVE_1/TPCE_1/TPGV_1-2/TPSS_1-12/*","",""},
    {"TPAD","inner pad row","HALL_1/CAVE_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d","tpc",""},// <+++
    {"TPA1","outer pad row","HALL_1/CAVE_1/TPCE_1/TPGV_%d/TPSS_%d/TPA1_%d","tpc",""}
  };
  _padPlane = gStTpcDb->PadPlaneGeometry();
  if (!_padPlane) 
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- _padPlane==0");
  
  _dimensions = gStTpcDb->Dimensions();
  if (!_dimensions)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- _dimensions==0");
  
  // change to +1 instead of +2 to remove the ofc.
  Int_t nSystems = sizeof(TpcVolumes)/sizeof(VolumeMap_t);
  unsigned int nRows = _padPlane->numberOfRows() + (nSystems - 2);// without inner and outer padrows
  setNRows(nRows);
  UInt_t row;
#if 0
  Int_t NoStiSectors = 24;
#else
  Int_t NoStiSectors = 12;
#endif
  for (row = 0; row < nRows - 1; row++) setNSectors(row,NoStiSectors);
  row = nRows - 1;
  setNSectors(row,1);
  Int_t kTIFC = -1, kTPAD = -1;
  for (Int_t system = 0; system < nSystems; system++) {
    TString name(TpcVolumes[system].name);
    if (name == "TIFC")  kTIFC = system;
    if (name == "TPAD") {kTPAD = system; break;}
  }
  // Get Materials
  TGeoVolume *volT = gGeoManager->GetVolume("TPAD");
  TGeoMaterial *mat = volT->GetMaterial();
  Double_t PotI = StiVMCToolKit::GetPotI(mat);
  StiMaterial* _gas = add(new StiMaterial(mat->GetName(),
					  mat->GetZ(),
					  mat->GetA(),
					  mat->GetDensity(),
					  mat->GetDensity()*mat->GetRadLen(),
					  PotI));
  Double_t ionization = _gas->getIonization();
  StiElossCalculator *gasElossCalculator =  new StiElossCalculator(_gas->getZOverA(), ionization*ionization, _gas->getA(), _gas->getZ(), _gas->getDensity());
  // IFC
  Double_t dPhi = 2*TMath::Pi();
  Int_t sector = 0;
  volT = gGeoManager->GetVolume(TpcVolumes[kTIFC].name);
  Double_t xyzM[3];
  TGeoShape *newshape = 0;
  TGeoMedium* newmed = 0;
  StiVMCToolKit::MakeAverageVolume(volT, newshape, newmed, xyzM);
  TGeoMaterial *fcMaterial = newmed->GetMaterial();
  PotI = StiVMCToolKit::GetPotI(fcMaterial);
  _fcMaterial  = add(new StiMaterial(fcMaterial->GetName(),
				     fcMaterial->GetZ(),
				     fcMaterial->GetA(),
				     fcMaterial->GetDensity(),
				     fcMaterial->GetDensity()*fcMaterial->GetRadLen(),
				     PotI));
  ionization = _fcMaterial->getIonization();
  StiElossCalculator * fcElossCalculator = new StiElossCalculator(_fcMaterial->getZOverA(), ionization*ionization, _fcMaterial->getA(), _fcMaterial->getZ(), _fcMaterial->getDensity());
  
  TGeoTube *fcShape = (TGeoTube *) newshape;
  Double_t Rmax = fcShape->GetRmax();
  Double_t Rmin = fcShape->GetRmin();
  Double_t dZ   = fcShape->GetDz();
  Double_t radius = (Rmin + Rmax)/2;
  StiCylindricalShape *FCShape = 
    new StiCylindricalShape(TpcVolumes[kTIFC].comment, // Name
			    dZ,                        // halfDepth
			    Rmax-Rmin,                 // thickness
			    Rmax,                      // outerRadius
			    dPhi);                     // openingAngle
  add(FCShape);
  StiPlacement *p = new StiPlacement;
  TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(TpcVolumes[kTIFC].path);
  TGeoHMatrix  *hmat   = nodeP->GetMatrix();
  Double_t *xyz = hmat->GetTranslation();
  p->setZcenter(xyz[2]);
  p->setLayerRadius(radius);
  p->setLayerAngle(sector*dPhi);
  p->setNormalRep(sector*dPhi, radius, 0.);
  p->setRegion(StiPlacement::kMidRapidity);
  StiDetector *fcDet = _detectorFactory->getInstance();
  TString nameP(TpcVolumes[kTIFC].path);
  nameP.ReplaceAll("HALL_1/CAVE_1/","");
  nameP.Resize(30); nameP.Strip();
  fcDet->setName(nameP.Data());
  fcDet->setIsOn(true);
  fcDet->setIsActive(new StiNeverActiveFunctor);
  fcDet->setIsContinuousMedium(false);
  fcDet->setIsDiscreteScatterer(true);
  fcDet->setShape(FCShape);
  fcDet->setPlacement(p);
  fcDet->setGas(_gas);
  fcDet->setMaterial(_fcMaterial);
  fcDet->setElossCalculator(fcElossCalculator);
  add(row,sector,fcDet);
  StDetectorDbTpcRDOMasks *s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  StiPlanarShape *pShape;
  //Active TPC padrows 
  //  double radToDeg = 180./3.1415927;
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  local2GlobalRotation; 
  StMatrixD  unit(3,3,1);
  StThreeVectorD RowPosition;
  unsigned int _nInnerPadrows = _padPlane->numberOfInnerRows();
  for(row = 0; row<45; row++)    {
    //Nominal pad row information.
    // create properties shared by all sectors in this padrow
    float fRadius = _padPlane->radialDistanceAtRow(row+1);
    TString name(Form("Tpc/Padrow_%d", row));
    pShape = new StiPlanarShape;
    if (!pShape)
      throw runtime_error("StiTpcDetectorBuilder::buildDetectors() - FATAL - pShape==0||ofcShape==0");
    Double_t dZ = 0;
    if(row<_nInnerPadrows) {
      pShape->setThickness(_padPlane->innerSectorPadLength());
      dZ = _dimensions->innerEffectiveDriftDistance()/2;
    }   
    else {
      pShape->setThickness(_padPlane->outerSectorPadLength());
      dZ = _dimensions->outerEffectiveDriftDistance()/2;
    }
    pShape->setHalfDepth(dZ*24/NoStiSectors);
    pShape->setHalfWidth(_padPlane->PadPitchAtRow(row+1) * _padPlane->numberOfPadsAtRow(row+1) / 2.);
    pShape->setName(name.Data());
    for(unsigned int sector = 0; sector<getNSectors(); sector++) {
      //Retrieve position and orientation of the TPC pad rows from the database.
      StTpcLocalSectorDirection  dirLS[3];
      dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,sector+1,row+1);
      dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,sector+1,row+1);
      dirLS[2] = StTpcLocalSectorDirection(0.,0.,1.,sector+1,row+1);
      local2GlobalRotation = unit;
      for (int i = 0; i < 3; i++) {
	//	if (debug>1) cout << "dirLS\t" << dirLS[i] << endl;
#ifndef TPC_IDEAL_GEOM 
	StTpcLocalDirection        dirL;      
	StTpcLocalSectorAlignedDirection  dirLSA;
	transform(dirLS[i],dirLSA);//   if (debug>1) cout << "dirLSA\t" << dirLSA << endl;
	transform(dirLSA,dirL);    //   if (debug>1) cout << "dirL\t" << dirL << endl;
	StGlobalDirection          dirG;
	transform(dirL,dirG);//      if (debug>1) cout << "dirG\t" << dirG << endl;
#else
	StTpcLocalDirection  dirG;
	transform(dirLS[i],dirG);
#endif
	local2GlobalRotation(i+1,1) = dirG.position().x();
	local2GlobalRotation(i+1,2) = dirG.position().y();
	local2GlobalRotation(i+1,3) = dirG.position().z();
      }
      //      if (debug>1) cout << "Local2GlobalRotation = " << local2GlobalRotation << endl;
      double y  = transform.yFromRow(row+1);
       StTpcLocalSectorCoordinate  lsCoord(0., y, dZ, sector+1, row+1);// if (debug>1) cout << lsCoord << endl;
#ifndef TPC_IDEAL_GEOM 
      StTpcLocalSectorAlignedCoordinate lsCoordA;  
      transform(lsCoord,lsCoordA);//                       if (debug>1) cout << lsCoordA << endl;                   
      StGlobalCoordinate  gCoord; 
      transform(lsCoordA, gCoord);//                       if (debug>1) cout << gCoord << endl;                   
#else  // Ideal geom
      StTpcLocalCoordinate gCoord;
      transform(lsCoord, gCoord);
#endif      
      //unit vector normal to the pad plane
      double nx = local2GlobalRotation(2,1);
      double ny = local2GlobalRotation(2,2);
      //      double nz = local2GlobalRotation(2,3);
      double nt = sqrt(nx*nx+ny*ny);
      double xc = gCoord.position().x();
      double yc = gCoord.position().y();
      double zc = gCoord.position().z();
      double rc = sqrt(xc*xc+yc*yc);
      double rn = xc*nx/nt + yc*ny/nt;
      double phic = atan2(yc,xc);
      double phi2 = asin((xc*ny-nx*yc)/(nt*rc));
      //      if (debug) cout << "row:"<<row<<" sector:"<<sector<<" fRadius:"<<fRadius<<" rn:"<<rn<<" rc:"<<rc<<" phi(nominal):"<<phiForTpcSector(sector)*radToDeg<<" phic:"<<phic*radToDeg<<" phi2:"<< phi2*radToDeg<<endl;
      // create unique detector properties (placement & name)
      StiPlacement *pPlacement = new StiPlacement;
      if (NoStiSectors == 12) zc = 0;
      pPlacement->setZcenter(zc);
      pPlacement->setCenterRep(phic, rn, phi2); 
      
      pPlacement->setLayerRadius(fRadius);
      //      pPlacement->setLayerAngle(phiForTpcSector(sector));
      pPlacement->setLayerAngle(phic);
      
      pPlacement->setRegion(StiPlacement::kMidRapidity);
      name = Form("Tpc/Padrow_%d/Sector_%d", row, sector);
      // fill in the detector object and save it in our vector
      StiDetector *pDetector = _detectorFactory->getInstance();
      pDetector->setName(name.Data());
      pDetector->setIsOn(true);
      
      int iRdo = rdoForPadrow(row+1);
      bool west = s_pRdoMasks->isOn(sector+1, iRdo);
      bool east = s_pRdoMasks->isOn( 24-(sector+1)%12, iRdo);
      // temp overide...
      //if (row==12 || 
      //(sector==3 && (row==13 || row==14 || row==15 || row==16 || row==17 || row==18 || row==19 || row==20)))
      if (row==12) {
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
      pDetector->setKey(1,row);
      pDetector->setKey(2,sector);
      add(row,sector,pDetector);
    }// for sector
  }// for row
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Done" << endl; 
}
