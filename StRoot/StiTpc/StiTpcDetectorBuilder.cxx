#include <assert.h>
#include <stdio.h>
#include "Stiostream.h"
#include <stdexcept>
#include "StDbUtilities/StTpcLocalCoordinate.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StTpcDb/StTpcDb.h"
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
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpcIsActiveFunctor.h"
//#include "Sti/StiElossCalculator.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StTpcDb/StTpcDb.h"
#include "StMatrixD.hh"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
//#define TPC_IDEAL_GEOM

StiTpcDetectorBuilder::StiTpcDetectorBuilder(Bool_t active)
  : StiDetectorBuilder("Tpc",active), _fcMaterial(0){}

StiTpcDetectorBuilder::~StiTpcDetectorBuilder() {}

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
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Started" << endl;
  assert(gStTpcDb);
  useVMCGeometry();
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Done" << endl;
}
//________________________________________________________________________________
void StiTpcDetectorBuilder::useVMCGeometry() 
{
  int debug = 0;

  int buildFlagDef = 0; // 0 - long, West+East together
  			// 1 - split,West&East separately
  St_tpcPadConfigC *tpcPadConfigC = St_tpcPadConfigC::instance();


  if (debug>1) StiVMCToolKit::SetDebug(1);
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);
  const VolumeMap_t TpcVolumes[] = {
    //  {"TPCE","the TPC system in STAR","HALL_1/CAVE_1/TPCE_1","",""},
    //  {"TPCW","the TPC supporting endcap Wheel","HALL_1/CAVE_1/TPCE_1/TPCW_1-2/*","",""},
    //  {"TPEA","one endcap placed in TPC","HALL_1/CAVE_1/TPCE_1/TPEA_1-2/*","",""},
    //  {"TPCM","the Central Membrane placed in TPC","HALL_1/CAVE_1/TPCE_1/TPCM_1","",""},
    //  {"TOFC","outer field cage - fill it with insulating gas already","HALL_1/CAVE_1/TPCE_1/TOFC_1/*","",""},
    {"TIFC","Inner Field Cage","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TIFC_1","",""},
    {"TOFC","Inner Field Cage","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TOFC_1","",""},
    {"TPAD","inner pad row","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d","tpc",""},// <+++
    {"TPA1","outer pad row","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPA1_%d","tpc",""},
    {"tpad","all pad rows","/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TpcSectorWhole_%d/TpcGas_1/TpcPadPlane_%d/tpad_%d","tpc"} // VMC
  };
  Bool_t newRefSystem = kFALSE;
  if (gGeoManager->GetVolume("TpcRefSys")) newRefSystem = kTRUE;

  // change to +1 instead of +2 to remove the ofc.
  int nRows = St_tpcPadConfigC::instance()->numberOfRows(20);// Only sensitive detectors; iTPC sector 20
  setNRows(nRows);
  int NostiSectors = 12;
  //  if (nRows != St_tpcPadConfigC::instance()->numberOfRows(1)) NostiSectors = 24;
  //  for (row = 1; row <= nRows; row++) setNSectors(row-1 ,NostiSectors);
  // Get Materials
  TGeoVolume *volT = gGeoManager->GetVolume("TPAD"); 
  if (! volT) volT = gGeoManager->GetVolume("tpad"); 
  assert (volT);
  TGeoMaterial *mat = volT->GetMaterial(); assert(mat); if (debug>1) mat->Print();
  double PotI = StiVMCToolKit::GetPotI(mat); if (debug>1) cout << "PotI " << PotI << endl;
  _gasMat = add(new StiMaterial(mat->GetName(),
				mat->GetZ(),
				mat->GetA(),
				mat->GetDensity(),
				mat->GetDensity()*mat->GetRadLen(),
				PotI));
//  double ionization = _gasMat->getIonization();
//   StiElossCalculator *gasElossCalculator =  new StiElossCalculator(_gasMat->getZOverA(), ionization*ionization,
// 								   _gasMat->getA(), _gasMat->getZ(), _gasMat->getDensity());
  StDetectorDbTpcRDOMasks *s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  StiPlanarShape *pShape;
  //Active TPC padrows
  //  double radToDeg = 180./3.1415927;
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  local2GlobalRotation;
  StMatrixD  unit(3,3,1);
  StThreeVectorD RowPosition;
#if 0
{
      int myInnNRows4  = tpcPadConfigC->innerPadRows(4);
      int myOutNRows4  = tpcPadConfigC->outerPadRows(4);
      int myInnNRows20  = tpcPadConfigC->innerPadRows(20);
      int myOutNRows20  = tpcPadConfigC->outerPadRows(20);
      int myNRows4  = tpcPadConfigC->numberOfRows(4);
      int myNRows10  = tpcPadConfigC->numberOfRows(20);

printf("4=%d %d 20=%d %d\n",myInnNRows4,myOutNRows4, myInnNRows20, myOutNRows20); 
printf("nRows 4=%d 20=%d\n",myNRows4,myNRows10);

      double fRadius413 = St_tpcPadConfigC::instance()->radialDistanceAtRow(4,13);
      double fRadius414 = St_tpcPadConfigC::instance()->radialDistanceAtRow(4,14);
      double fRadius2013 = St_tpcPadConfigC::instance()->radialDistanceAtRow(20,13);
      double fRadius2014 = St_tpcPadConfigC::instance()->radialDistanceAtRow(20,14);
      printf("Rad 413=%g 414=%g 2013=%g 2014=%g \n",fRadius413,fRadius414,fRadius2013,fRadius2014); 

      double innThick4  = tpcPadConfigC->innerSectorPadLength(4);
      double innThick20 = tpcPadConfigC->innerSectorPadLength(20);
      double outThick4  = tpcPadConfigC->outerSectorPadLength(4);
      double outThick20 = tpcPadConfigC->outerSectorPadLength(20);

printf("Thick 4=%g 20=%g 4=%g 20=%g\n",innThick4, innThick20,outThick4,outThick20);
      double dZInn4  = tpcPadConfigC->innerSectorPadPlaneZ(4);
      double dZInn20 = tpcPadConfigC->innerSectorPadPlaneZ(20);
      double dZOut4  = tpcPadConfigC->outerSectorPadPlaneZ(4);
      double dZOut20 = tpcPadConfigC->outerSectorPadPlaneZ(20);
      double dZInn10  = tpcPadConfigC->innerSectorPadPlaneZ(10);
      double dZOut14  = tpcPadConfigC->outerSectorPadPlaneZ(14);

printf("dZ inn4=%g inn20=%g out4=%g out20=%g inn10=%g out14=%g\n",dZInn4,dZInn20 ,dZOut4 ,dZOut20,dZInn10,dZOut14 );

///	pShape->setHalfWidth(St_tpcPadConfigC::instance()->PadPitchAtRow(sector,row) * St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row) / 2.);
      double pitch413  = tpcPadConfigC->PadPitchAtRow( 4,13);
      double pitch2013 = tpcPadConfigC->PadPitchAtRow(20,13);
      double pitch443  = tpcPadConfigC->PadPitchAtRow( 4,43);
      double pitch2043 = tpcPadConfigC->PadPitchAtRow(20,43);
printf("Pitch 413=%g 2013=%g 443=%g 2043=%g\n",pitch413, pitch2013, pitch443, pitch2043  );
      int nPads413  = tpcPadConfigC->numberOfPadsAtRow( 4,13);
      int nPads2013 = tpcPadConfigC->numberOfPadsAtRow(20,13);
      int nPads443  = tpcPadConfigC->numberOfPadsAtRow( 4,43);
      int nPads2043 = tpcPadConfigC->numberOfPadsAtRow(20,43);
printf("nPads 413=%d 2013=%d 443=%d 2043=%d\n",nPads413, nPads2013, nPads443, nPads2043  );

  for (int s:{4,20}) {
    int nR = tpcPadConfigC->numberOfRows(s);
    for(int iR=1;iR<=nR;iR++) {
      printf(" %d - %d  %d\n",s,iR,tpcPadConfigC->numberOfPadsAtRow(s,iR));
  } }
assert(0);
}
#endif //0
  int nRowsWE[2],sectorWE[2];
  for(int stiSector = 1; stiSector <= NostiSectors; stiSector++) {
    sectorWE[0] = stiSector;
    sectorWE[1] = 24-(stiSector)%12;
    nRowsWE[0]  = tpcPadConfigC->numberOfRows(sectorWE[0]);
    nRowsWE[1]  = tpcPadConfigC->numberOfRows(sectorWE[1]);
    int buildFlag = buildFlagDef;
    if (nRowsWE[0]!=nRowsWE[1]) buildFlag|=1;
    int nWE = (buildFlag) ? 2:1;

    for (int iWE=0;iWE<nWE;iWE++) {
      int mySector = sectorWE[iWE];
      int myInnNRows  = tpcPadConfigC->innerPadRows(mySector);
      for (int iRow=1;iRow<= nRowsWE[iWE];iRow++) {   
	float fRadius = St_tpcPadConfigC::instance()->radialDistanceAtRow(mySector,iRow);
	TString name(Form("Tpc/Sector_%d,Padrow_%d",mySector,iRow));
        int inner = (iRow<=myInnNRows);
        double dZ,Zshift=0,thick;
        if (inner) {
	  dZ= tpcPadConfigC->innerSectorPadPlaneZ(mySector);
          thick  = tpcPadConfigC->innerSectorPadLength(mySector);
        } else {
	  dZ= tpcPadConfigC->outerSectorPadPlaneZ(mySector);
          thick  = tpcPadConfigC->outerSectorPadLength(mySector);
        }
        double pitch = tpcPadConfigC->PadPitchAtRow(mySector,iRow);
        int nPads    = tpcPadConfigC->numberOfPadsAtRow(mySector,iRow);
        if (buildFlag&1) { //Split case
          dZ/=2;
	  Zshift = (iWE)? -dZ:dZ;  
	}
	pShape = new StiPlanarShape;
	pShape->setThickness(thick);
	pShape->setHalfDepth(dZ);
	pShape->setHalfWidth(pitch*nPads/2.);
	pShape->setName(name.Data()); 

	StiPlacement *pPlacement = new StiPlacement;
	pPlacement->setZcenter(Zshift);
	pPlacement->setLayerRadius(fRadius);
        double phi = angle(mySector);
	pPlacement->setLayerAngle(phi);
	pPlacement->setRegion(StiPlacement::kMidRapidity);
	pPlacement->setNormalRep(phi, fRadius, 0);
#if 0
	//Retrieve position and orientation of the TPC pad rows from the database.
	StTpcLocalSectorDirection  dirLS[3];
	dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,mySector,iRow);
	dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,mySector,iRow);
	dirLS[2] = StTpcLocalSectorDirection(0.,0.,1.,mySector,iRow);
	local2GlobalRotation = unit;
	for (int i = 0; i < 3; i++) {
	  //	if (debug>1) cout << "dirLS\t" << dirLS[i] << endl;
	  StTpcLocalDirection  dirG;
	  transform(dirLS[i],dirG);
	  local2GlobalRotation(i+1,1) = dirG.position().x();
	  local2GlobalRotation(i+1,2) = dirG.position().y();
	  local2GlobalRotation(i+1,3) = dirG.position().z();
	}
	//      if (debug>1) cout << "Local2GlobalRotation = " << local2GlobalRotation << endl;
	double y  = transform.yFromRow(mySector,iRow);
	StTpcLocalSectorCoordinate  lsCoord(0., y, dZ, mySector, iRow);// if (debug>1) cout << lsCoord << endl;
 // Ideal geom
	StTpcLocalCoordinate gCoord;
	transform(lsCoord, gCoord);
	//unit vector normal to the pad plane
	StThreeVectorD centerVector(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
	StThreeVectorD normalVector(local2GlobalRotation(2,1),
				    local2GlobalRotation(2,2),
				    local2GlobalRotation(2,3));
	double prod = centerVector*normalVector;
	if (prod < 0) normalVector *= -1;
	double phi  = centerVector.phi();
	double phiD = normalVector.phi();
	double r = centerVector.perp();
	StiPlacement *pPlacement = new StiPlacement;
	pPlacement->setZcenter(Zshift);
	pPlacement->setLayerRadius(fRadius);
	pPlacement->setLayerAngle(phi);
	pPlacement->setRegion(StiPlacement::kMidRapidity);
	pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD));
        int mySector = sector(pPlacement->getNormalRefAngle(),stiSector>12);
        assert(stiSector==mySector);
	assert(fabs(pPlacement->getNormalRefAngle()-pPlacement->getCenterRefAngle())<1e-2);
#endif	
	name = Form("Tpc/Padrow_%d/Sector_%d", iRow, mySector);
	// fill in the detector object and save it in our vector
	StiDetector *pDetector = _detectorFactory->getInstance();
	pDetector->setName(name.Data());
	pDetector->setIsOn(kTRUE);
	Bool_t west = kTRUE;
	Bool_t east = kTRUE;
#if 0
	if (NostiSectors == 12 && nRows == 45) { // ! iTpx
	  Bool_t west = s_pRdoMasks->isRowOn(mySector, iRow);
	  Bool_t east = s_pRdoMasks->isRowOn( 24-(mySector)%12, iRow);
	  if (west) {
	    int sec = sector;
	    west = St_tpcAnodeHVavgC::instance()->livePadrow(sec,row) &&
	      St_tpcPadGainT0BC::instance()->livePadrow(sec,row);
	  }
	  if (east) {
	    int sec = 24-(sector)%12;
	    east = St_tpcAnodeHVavgC::instance()->livePadrow(sec,row) &&
	      St_tpcPadGainT0BC::instance()->livePadrow(sec,row);
	  }
	}
#endif
	pDetector->setIsActive(new StiTpcIsActiveFunctor(_active,west,east));
	pDetector->setIsContinuousMedium(kTRUE);
	pDetector->setIsDiscreteScatterer(kFALSE);
	pDetector->setMaterial(_gasMat);
	pDetector->setGas(_gasMat);
	pDetector->setShape(pShape);
	pDetector->setPlacement(pPlacement);
	if (inner)
	  pDetector->setHitErrorCalculator(StiTpcInnerHitErrorCalculator::instance());
	else
	  pDetector->setHitErrorCalculator(StiTpcOuterHitErrorCalculator::instance());
	//      pDetector->setElossCalculator(gasElossCalculator);
	pDetector->setKey(1,iRow);
	pDetector->setKey(2,mySector);

printf("TpcName = %s sect=%d raw=%d Zc = %g Zl = %g\n"
       ,pDetector->getName().c_str(),mySector,iRow,Zshift,dZ);



	add(iRow-1,mySector-1,pDetector); 
        if (nWE==2) continue;
        name+="*";
        pDetector->setName(name.Data());
	add(iRow-1,sectorWE[1]-1,pDetector); 
      }// for row
    }// Tpc halves
  }// for sector
  for (int i = 0; i < 2; i++) {
    if (! gGeoManager->GetVolume(TpcVolumes[i].name)) continue;
    gGeoManager->RestoreMasterVolume();
    gGeoManager->CdTop();
    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    TString path = TpcVolumes[i].path;
    if (! newRefSystem) path.ReplaceAll("/TpcRefSys_1","");
    while (path.Contains("_%d")) {
      path = gSystem->DirName(path);
    }
    if (! gGeoManager->cd(path)) continue;
    nodeT = gGeoManager->GetCurrentNode();
    if (! nodeT) continue;
    path = gGeoManager->GetPath();
    StiVMCToolKit::LoopOverNodes(nodeT, path, TpcVolumes[i].name, MakeAverageVolume);
  }
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Done" << endl;
}
//________________________________________________________________________________
int StiTpcDetectorBuilder::sector(double ang,int east)
{
  double gang = ang/M_PI*180;
  double d = -(gang)/30+3;
  if (d<0.5) d+=12; if (d>12.5) d-=12;
  int sec = d+0.5;
  if (east) sec = 24-(sec)%12;
  return sec;
}
//________________________________________________________________________________
double StiTpcDetectorBuilder::angle(int sec)
{
  if (sec>12) sec = 12-sec%12;
  double ang = (3-sec)*(M_PI/6);
  return ang;
}

