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
        name+="*"; pDetector->setName(name.Data());
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

