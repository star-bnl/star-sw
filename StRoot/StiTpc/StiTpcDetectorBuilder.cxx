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
#if 0
  if (!gStTpcDb)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() -E- gStTpcDb==0");
#else
  assert(gStTpcDb);
#endif
  useVMCGeometry();
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Done" << endl;
}
//________________________________________________________________________________
void StiTpcDetectorBuilder::useVMCGeometry() {
  Int_t debug = 0;

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
  Int_t nRows = St_tpcPadConfigC::instance()->numberOfRows(20);// Only sensitive detectors; iTPC sector 20
  setNRows(nRows);
  Int_t row;
  Int_t NoStiSectors = 12;
  //  if (nRows != St_tpcPadConfigC::instance()->numberOfRows(1)) NoStiSectors = 24;
  //  for (row = 1; row <= nRows; row++) setNSectors(row-1 ,NoStiSectors);
  // Get Materials
  TGeoVolume *volT = gGeoManager->GetVolume("TPAD"); 
  if (! volT) volT = gGeoManager->GetVolume("tpad"); 
  assert (volT);
  TGeoMaterial *mat = volT->GetMaterial(); assert(mat); if (debug>1) mat->Print();
  Double_t PotI = StiVMCToolKit::GetPotI(mat); if (debug>1) cout << "PotI " << PotI << endl;
  _gasMat = add(new StiMaterial(mat->GetName(),
				mat->GetZ(),
				mat->GetA(),
				mat->GetDensity(),
				mat->GetDensity()*mat->GetRadLen(),
				PotI));
//  Double_t ionization = _gasMat->getIonization();
//   StiElossCalculator *gasElossCalculator =  new StiElossCalculator(_gasMat->getZOverA(), ionization*ionization,
// 								   _gasMat->getA(), _gasMat->getZ(), _gasMat->getDensity());
  StDetectorDbTpcRDOMasks *s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  StiPlanarShape *pShape;
  //Active TPC padrows
  //  Double_t radToDeg = 180./3.1415927;
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  local2GlobalRotation;
  StMatrixD  unit(3,3,1);
  StThreeVectorD RowPosition;
  for(Int_t StiSector = 1; StiSector <= NoStiSectors; StiSector++) {
    Int_t sectorW = StiSector;
    Int_t sectorE = 24-(StiSector)%12;
    Int_t nRowsW = St_tpcPadConfigC::instance()->numberOfRows(sectorW);
    Int_t nRowsE = St_tpcPadConfigC::instance()->numberOfRows(sectorE);
    nRows = TMath::Max(nRowsW, nRowsE);
    for(Int_t rowW = nRowsW, rowE = nRowsE; rowW > 0 || rowE > 0; rowW--, rowE--) {
      Int_t StiRowW = StiRow(sectorW,rowW);
      Int_t StiRowE = StiRow(sectorE,rowE);
      Int_t StiRows[2] = {StiRowW, StiRowE}; 
      Int_t sectors[2] = {sectorW, sectorE};
      Int_t rows[2]= {rowW, rowE}; 
      Int_t Nsectors = 1;
      if (rowW > 0 && rowE > 0 && StiRowW != StiRowE) Nsectors = 2;
      if (rowW <= 0) {sectors[0] = sectorE; StiRows[0] = StiRowE; rows[0] = rowE;}
      for (Int_t k = 0; k < Nsectors; k++) {
	Int_t sector = sectors[k];
	Int_t StiRow = StiRows[k];
	row = rows[k];
	assert(row > 0);
#if 0
	if (StiRow-1 < (Int_t) _detectors.size()) {
	  if (_detectors[StiRow-1].size() && _detectors[StiRow-1][StiSector-1]) {
	    static Int_t skipped = 0;
	    skipped++;
	    continue;
	  }
	}
#endif
	//Nominal pad row information.
	// create properties shared by all sectors in this padrow
	float fRadius = St_tpcPadConfigC::instance()->radialDistanceAtRow(sector,row);
	TString name(Form("Tpc/Sector_%d,Padrow_%d", StiSector,StiRow));
	pShape = new StiPlanarShape;
	assert(pShape);
	Double_t dZ = 0;
	Double_t Zshift = 0; // 
	Double_t Zdepth = dZ;
	Double_t zc = 0;
	pShape->setThickness(St_tpcPadConfigC::instance()->innerSectorPadLength(sector));
	dZ = St_tpcPadConfigC::instance()->innerSectorPadPlaneZ(sector);
	if(Nsectors == 2) {
	  Zdepth = (dZ + Zshift)/2;
	  zc     = (dZ - Zshift)/2;
	  if (sector > 12) zc = -zc;
	} else {
	  Zdepth = dZ;
	  zc     =  0;
	}
	pShape->setHalfDepth(Zdepth);
	pShape->setHalfWidth(St_tpcPadConfigC::instance()->PadPitchAtRow(sector,row) * St_tpcPadConfigC::instance()->numberOfPadsAtRow(sector,row) / 2.);
	pShape->setName(name.Data()); if (debug>1) cout << *pShape << endl;
	//Retrieve position and orientation of the TPC pad rows from the database.
	StTpcLocalSectorDirection  dirLS[3];
	dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,sector,row);
	dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,sector,row);
	dirLS[2] = StTpcLocalSectorDirection(0.,0.,1.,sector,row);
	local2GlobalRotation = unit;
	for (Int_t i = 0; i < 3; i++) {
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
	Double_t y  = transform.yFromRow(sector,row);
	StTpcLocalSectorCoordinate  lsCoord(0., y, dZ, sector, row);// if (debug>1) cout << lsCoord << endl;
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
	StThreeVectorD centerVector(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
	StThreeVectorD normalVector(local2GlobalRotation(2,1),
				    local2GlobalRotation(2,2),
				    local2GlobalRotation(2,3));
	Double_t prod = centerVector*normalVector;
	if (prod < 0) normalVector *= -1;
	Double_t phi  = centerVector.phi();
	Double_t phiD = normalVector.phi();
	Double_t r = centerVector.perp();
	StiPlacement *pPlacement = new StiPlacement;
	pPlacement->setZcenter(zc);
	pPlacement->setLayerRadius(fRadius);
	pPlacement->setLayerAngle(phi);
	pPlacement->setRegion(StiPlacement::kMidRapidity);
	pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD));
	name = Form("Tpc/Padrow_%d/Sector_%d", row, sector);
	// fill in the detector object and save it in our vector
	StiDetector *pDetector = _detectorFactory->getInstance();
	pDetector->setName(name.Data());
	pDetector->setIsOn(kTRUE);
	Bool_t west = kTRUE;
	Bool_t east = kTRUE;
	if (NoStiSectors == 12 && nRows == 45) { // ! iTpx
	  Bool_t west = s_pRdoMasks->isRowOn(sector, row);
	  Bool_t east = s_pRdoMasks->isRowOn( 24-(sector)%12, row);
	  if (west) {
	    Int_t sec = sector;
	    west = St_tpcAnodeHVavgC::instance()->livePadrow(sec,row) &&
	      St_tpcPadGainT0BC::instance()->livePadrow(sec,row);
	  }
	  if (east) {
	    Int_t sec = 24-(sector)%12;
	    east = St_tpcAnodeHVavgC::instance()->livePadrow(sec,row) &&
	      St_tpcPadGainT0BC::instance()->livePadrow(sec,row);
	  }
	}
	pDetector->setIsActive(new StiTpcIsActiveFunctor(_active,west,east));
	pDetector->setIsContinuousMedium(kTRUE);
	pDetector->setIsDiscreteScatterer(kFALSE);
	pDetector->setMaterial(_gasMat);
	pDetector->setGas(_gasMat);
	pDetector->setShape(pShape);
	pDetector->setPlacement(pPlacement);
	Int_t nInnerPadrows = St_tpcPadConfigC::instance()->numberOfInnerRows(sector);
	if (row <= nInnerPadrows)
	  pDetector->setHitErrorCalculator(StiTpcInnerHitErrorCalculator::instance());
	else
	  pDetector->setHitErrorCalculator(StiTpcOuterHitErrorCalculator::instance());
	//      pDetector->setElossCalculator(gasElossCalculator);
	pDetector->setKey(1,StiRow);
	pDetector->setKey(2,StiSector);
	add(StiRow-1,StiSector-1,pDetector); if (debug>1) cout << *pDetector << endl;
      }// for row
    }// Tpc halves
  }// for sector
  for (Int_t i = 0; i < 2; i++) {
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
Int_t StiTpcDetectorBuilder::StiRow(Int_t sector, Int_t padrow) {
  // StiRow : [1 ; nInnerPadrowsW] 
  //          [nInnerPadrowsW+1: nInnerPadrowsW+nInnerPadrowsE]
  //          [nInnerPadrowsW+nInnerPadrowsE+1:nInnerPadrowsW+nInnerPadrowsE+nOuterPadrowsW]
  Int_t StiSector = sector;
  if (sector > 12) StiSector = 12 - (sector-12)%12;
  Int_t sectorW = StiSector;
  Int_t sectorE = 24-(StiSector)%12;
  Int_t nInnerPadrowsW = St_tpcPadConfigC::instance()->numberOfInnerRows(sectorW);
  Int_t nInnerPadrowsE = St_tpcPadConfigC::instance()->numberOfInnerRows(sectorE);
  Int_t nOuterPadrowsW = St_tpcPadConfigC::instance()->numberOfOuterRows(sectorW);
  //  Int_t nOuterPadrowsE = St_tpcPadConfigC::instance()->numberOfOuterRows(sectorE);
  if (nInnerPadrowsW == nInnerPadrowsE) return padrow;
  if (sector <= 12) {
    if (padrow <= nInnerPadrowsW) return padrow;
    if (padrow > nInnerPadrowsW + nOuterPadrowsW) return 0;
    return nInnerPadrowsW + nInnerPadrowsE + (padrow - nInnerPadrowsW);
  } else {
    if (padrow <= nInnerPadrowsE) return padrow + nInnerPadrowsW;
    return padrow + nInnerPadrowsW;
  }
  return 0;
}
//________________________________________________________________________________
void StiTpcDetectorBuilder::testStiRow() {
  for (Int_t row = 72; row > 0; row--) {
    Int_t StiRow4  = StiRow(4,row);
    Int_t StiRow20 = StiRow(20,row);
    cout << "row = " << row << "\tStiRow4 = " << StiRow4 << "\tStiRow20 = " << StiRow20 << endl;
    
  }
}
