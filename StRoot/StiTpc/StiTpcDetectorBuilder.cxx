#include <algorithm>
#include <assert.h>
#include <stdio.h>
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
#include "Rtypes.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StiTpcDetectorBuilder.h"
#include "StiTpc/StiTpcIsActiveFunctor.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StTpcDb/StTpcDb.h"
#include "StMatrixD.hh"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0BC.h"
//#define TPC_IDEAL_GEOM

std::set<StiTpcDetectorBuilder::StiLayer> StiTpcDetectorBuilder::sStiLayers{};


StiTpcDetectorBuilder::StiTpcDetectorBuilder(Bool_t active, bool active_iTpc)
  : StiDetectorBuilder("Tpc",active), _active_iTpc(active_iTpc) {}

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
  Int_t debug = 0;

  if (debug>1) StiVMCToolKit::SetDebug(1);
  cout << "StiTpcDetectorBuilder::buildDetectors() -I- Use VMC geometry" << endl;
  SetCurrentDetectorBuilder(this);

  // Get Materials
  TGeoVolume *volT = gGeoManager->GetVolume("TPAD"); 
  if (! volT) volT = gGeoManager->GetVolume("tpad"); 
  assert(volT);
  TGeoMaterial *mat = volT->GetMaterial();
  assert(mat);
  if (debug>1) mat->Print();
  Double_t PotI = StiVMCToolKit::GetPotI(mat);
  if (debug>1) cout << "PotI " << PotI << endl;
  _gasMat = add(new StiMaterial(mat->GetName(),
                                mat->GetZ(),
                                mat->GetA(),
                                mat->GetDensity(),
                                mat->GetDensity()*mat->GetRadLen(),
                                PotI));

  // Create active Sti volumes for TPC sensitive layers
  StiTpcDetectorBuilder::buildStiLayerMap();

  for(const StiLayer& stiLayer : sStiLayers)
  {
    StiPlanarShape* pShape = constructTpcPadrowShape(stiLayer);
    StiDetector* pDetector = constructTpcPadrowDetector(stiLayer, pShape);

    add(stiLayer.sti_padrow_id, stiLayer.sti_sector_id, pDetector);
    if (debug>1) cout << *pDetector << endl;
  }

  // Create inactive Sti volumes by averaging material in TPC inner and outer field cages
  const VolumeMap_t TpcVolumes[] = {
    {"TIFC","Inner Field Cage","HALL_1/CAVE_1/TPCE_1/TIFC_1","",""},
    {"TOFC","Inner Field Cage","HALL_1/CAVE_1/TPCE_1/TOFC_1","",""},
    {"TIFC","Inner Field Cage","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TIFC_1","",""},
    {"TOFC","Inner Field Cage","HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TOFC_1","",""},
  };

  TString check_path("HALL_1/CAVE_1/TpcRefSys_1/TPCE_1");
  bool newRefSystem = gGeoManager->cd(check_path) ? true : false;

  for (Int_t i = 0; i < 4; i++) {
    TString path = TpcVolumes[i].path;

    if ( newRefSystem && !path.Contains("TpcRefSys")) continue;
    if (!newRefSystem &&  path.Contains("TpcRefSys")) continue;
    if (!gGeoManager->cd(path)) {
      LOG_WARN << "Skipping non-existing geometry path " << path << endm;
      continue;
    }

    TGeoNode *nodeT = gGeoManager->GetCurrentNode();
    StiVMCToolKit::LoopOverNodes(nodeT, path, TpcVolumes[i].name, MakeAverageVolume);
  }
}


StiDetector* StiTpcDetectorBuilder::constructTpcPadrowDetector(StiLayer stiLayer, StiPlanarShape* pShape) const
{
  int tpc_sector_id = stiLayer.tpc_sector();
  int tpc_padrow_id = stiLayer.tpc_padrow();

  StDetectorDbTpcRDOMasks *s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  St_tpcPadConfigC& tpcPadCfg = *St_tpcPadConfigC::instance();
  UInt_t nRows = tpcPadCfg.numberOfRows(tpc_sector_id);// Only sensitive detectors
  UInt_t nInnerPadrows = tpcPadCfg.numberOfInnerRows(tpc_sector_id);
  //Nominal pad row information.
  // create properties shared by all sectors in this padrow
  float fRadius = tpcPadCfg.radialDistanceAtRow(tpc_sector_id, tpc_padrow_id);
  StTpcCoordinateTransform transform(gStTpcDb);
  StMatrixD  local2GlobalRotation;
  StMatrixD  unit(3,3,1);
  // The length of the Sti layer shape is used to place it in the rigth position
  // along z. We have to multiply by 0.5 to revert the effect of the old
  // implementation bug when the length was doubled
  Double_t dZ = pShape->getHalfDepth()*0.5;

  //Retrieve position and orientation of the TPC pad rows from the database.
  StTpcLocalSectorDirection  dirLS[3];
  dirLS[0] = StTpcLocalSectorDirection(1.,0.,0.,tpc_sector_id,tpc_padrow_id);
  dirLS[1] = StTpcLocalSectorDirection(0.,1.,0.,tpc_sector_id,tpc_padrow_id);
  dirLS[2] = StTpcLocalSectorDirection(0.,0.,1.,tpc_sector_id,tpc_padrow_id);
  local2GlobalRotation = unit;
  for (Int_t i = 0; i < 3; i++) {
    // if (debug>1) cout << "dirLS\t" << dirLS[i] << endl;
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
  Double_t y  = transform.yFromRow(tpc_sector_id, tpc_padrow_id);
  StTpcLocalSectorCoordinate  lsCoord(0., y, dZ, tpc_sector_id, tpc_padrow_id);// if (debug>1) cout << lsCoord << endl;
#ifndef TPC_IDEAL_GEOM
  StTpcLocalSectorAlignedCoordinate lsCoordA;
  transform(lsCoord,lsCoordA);//                       if (debug>1) cout << lsCoordA << endl;
  StGlobalCoordinate  gCoord;
  transform(lsCoordA, gCoord);//                       if (debug>1) cout << gCoord << endl;
#else  // Ideal geom
  StTpcLocalCoordinate gCoord;
  transform(lsCoord, gCoord);
#endif
  // unit vector normal to the pad plane
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
  Double_t zc = 0;

  if ( stiLayer.represents_only(StiLayer::West) ) zc =  2*dZ;
  if ( stiLayer.represents_only(StiLayer::East) ) zc = -2*dZ;

  pPlacement->setZcenter(zc);
  pPlacement->setLayerRadius(fRadius);
  pPlacement->setLayerAngle(phi);
  pPlacement->setRegion(StiPlacement::kMidRapidity);
  pPlacement->setNormalRep(phiD, r*TMath::Cos(phi-phiD), r*TMath::Sin(phi-phiD));
  TString name = Form("Tpc/Padrow_%d/Sector_%d", stiLayer.sti_padrow_id, stiLayer.sti_sector_id);
  // fill in the detector object and save it in our vector
  StiDetector *pDetector = _detectorFactory->getInstance();
  pDetector->setName(name.Data());
  pDetector->setIsOn(kTRUE);
  Bool_t west = kTRUE;
  Bool_t east = kTRUE;
  if (nRows == 45) { // ! iTpx

    int tpc_sector_id_west = stiLayer.tpc_sector(StiLayer::West);
    int tpc_sector_id_east = stiLayer.tpc_sector(StiLayer::East);

    Int_t iRdo  = s_pRdoMasks->rdoForPadrow(tpc_padrow_id);
    Bool_t west = tpc_sector_id_west > 0 && s_pRdoMasks->isOn(tpc_sector_id_west, iRdo);
    Bool_t east = tpc_sector_id_east > 0 && s_pRdoMasks->isOn(tpc_sector_id_east, iRdo);

    if (west) {
      west = St_tpcAnodeHVavgC::instance()->livePadrow(tpc_sector_id_west,tpc_padrow_id) &&
             St_tpcPadGainT0BC::instance()->livePadrow(tpc_sector_id_west,tpc_padrow_id);
    }
    if (east) {
      east = St_tpcAnodeHVavgC::instance()->livePadrow(tpc_sector_id_east,tpc_padrow_id) &&
             St_tpcPadGainT0BC::instance()->livePadrow(tpc_sector_id_east,tpc_padrow_id);
    }
  }

  StiIsActiveFunctor* activator = nullptr;

  if ( tpcPadCfg.isiTpcPadRow(tpc_sector_id, tpc_padrow_id) ) {
    //pDetector->setGroupId(kiTpcId);
    pDetector->setGroupId(kTpcId); // Y. Fisyak and I. Chakaberia approach is not to use kiTpcId
    activator = _active_iTpc ? new StiTpcIsActiveFunctor(true,west,east) :
                               new StiTpcIsActiveFunctor(false,west,east);
  }
  else {
    pDetector->setGroupId(kTpcId);
    activator = new StiTpcIsActiveFunctor(_active,west,east);
  }

  pDetector->setIsActive(activator);
  pDetector->setIsContinuousMedium(kTRUE);
  pDetector->setIsDiscreteScatterer(kFALSE);
  pDetector->setMaterial(_gasMat);
  pDetector->setGas(_gasMat);
  pDetector->setShape(pShape);
  pDetector->setPlacement(pPlacement);

  if (tpc_padrow_id <= nInnerPadrows)
    pDetector->setHitErrorCalculator(StiTpcInnerHitErrorCalculator::instance());
  else
    pDetector->setHitErrorCalculator(StiTpcOuterHitErrorCalculator::instance());

  pDetector->setKey(1,stiLayer.sti_padrow_id);
  pDetector->setKey(2,stiLayer.sti_sector_id);

  return pDetector;
}


StiPlanarShape* StiTpcDetectorBuilder::constructTpcPadrowShape(StiLayer stiLayer) const
{
  int tpc_sector_id = stiLayer.tpc_sector();
  int tpc_padrow_id = stiLayer.tpc_padrow();

  St_tpcPadConfigC& tpcPadCfg = *St_tpcPadConfigC::instance();
  UInt_t nInnerPadrows = tpcPadCfg.numberOfInnerRows(tpc_sector_id);

  TString name = Form("Tpc/Padrow_%d/Sector_%d", stiLayer.sti_padrow_id, stiLayer.sti_sector_id);
  StiPlanarShape* pShape = new StiPlanarShape;

  if (!pShape)
    throw runtime_error("StiTpcDetectorBuilder::buildDetectors() - FATAL - pShape==0||ofcShape==0");

  Double_t dZ = 0;
  if(tpc_padrow_id <= nInnerPadrows) {
    pShape->setThickness(tpcPadCfg.innerSectorPadLength(tpc_sector_id));
    dZ = tpcPadCfg.innerSectorPadPlaneZ(tpc_sector_id);
  }
  else {
    pShape->setThickness(tpcPadCfg.outerSectorPadLength(tpc_sector_id));
    dZ = tpcPadCfg.outerSectorPadPlaneZ(tpc_sector_id);
  }

  // Check if stiLayer represents only one half of TPC layer
  if ( stiLayer.represents_only(StiLayer::West) ||
       stiLayer.represents_only(StiLayer::East) )
  {
    dZ *= 0.5;
  }

  // The length is doubled to match the old implementation where the value
  // depended on the number of TPC sectors. Without the factor 2 we loose prompt
  // hits which can be outside of the physical volume
  pShape->setHalfDepth(dZ*2);
  pShape->setHalfWidth(tpcPadCfg.PadPitchAtRow(tpc_sector_id, tpc_padrow_id) *
                       tpcPadCfg.numberOfPadsAtRow(tpc_sector_id, tpc_padrow_id) / 2.);
  pShape->setName(name.Data());

  if (StiVMCToolKit::Debug()>1) cout << *pShape << endl;

  return pShape;
}



double StiTpcDetectorBuilder::StiLayer::radial_distance() const
{
  St_tpcPadConfigC& tpcPadCfg = *St_tpcPadConfigC::instance();

  double delta = 0;

  if (sti_split_in_half)
    delta = represents_only(StiLayer::East) ? +0.1 : (represents_only(StiLayer::West) ? -0.1 : 0);

  return tpcPadCfg.radialDistanceAtRow(tpc_sector(), tpc_padrow()) + delta;
}



bool StiTpcDetectorBuilder::StiLayer::operator< (const StiLayer& other) const
{
  double radius       = radial_distance();
  double radius_other = other.radial_distance();

  bool result =
         ( radius <  radius_other ) ||
         ( radius == radius_other && sti_sector_id <  other.sti_sector_id );

  return result;
}



std::ostream& operator<<(std::ostream& os, const StiTpcDetectorBuilder::StiLayer& stiLayer)
{
  St_tpcPadConfigC& tpcPadCfg = *St_tpcPadConfigC::instance();

  double radius = tpcPadCfg.radialDistanceAtRow(stiLayer.tpc_sector(), stiLayer.tpc_padrow());

  os << radius << ",\t" << stiLayer.radial_distance() << ";\t"
     << stiLayer.tpc_sector_id[0] << ",\t" << stiLayer.tpc_padrow_id[0] << ";\t"
     << stiLayer.tpc_sector_id[1] << ",\t" << stiLayer.tpc_padrow_id[1] << "\t-->\t"
     << stiLayer.sti_sector_id    << ",\t" << stiLayer.sti_padrow_id;

  return os;
}



std::pair<int, int> StiTpcDetectorBuilder::toStiLayer(const int tpc_sector, const int tpc_padrow)
{
  auto find_tpc_sector = [tpc_sector, tpc_padrow](const StiLayer& sl)
  {
    StiLayer::TpcHalf half = (tpc_sector <= 12 ? StiLayer::West : StiLayer::East);
    return sl.tpc_sector_id[half] == tpc_sector && sl.tpc_padrow_id[half] == tpc_padrow;
  };

  auto stiLayerIter = std::find_if(sStiLayers.begin(), sStiLayers.end(), find_tpc_sector);

  return stiLayerIter != sStiLayers.end() ?
    std::make_pair(stiLayerIter->sti_sector_id, stiLayerIter->sti_padrow_id) :
    std::pair<int, int>(-1, -1);
}



void StiTpcDetectorBuilder::buildStiLayerMap()
{
  St_tpcPadConfigC& tpcPadCfg = *St_tpcPadConfigC::instance();

  sStiLayers.clear();

  for(int sector = 1; sector <= 24; sector++)
  {
    for(int row = 1; row <= tpcPadCfg.numberOfRows(sector); row++)
    {
      std::set<StiLayer>::iterator stiLayerIter;
      bool inserted;
      std::tie(stiLayerIter, inserted) = sStiLayers.insert( StiLayer(sector, row) );

      if (!inserted) stiLayerIter->update_tpc_id(sector, row);
    }
  }

  // Now we loop over the sorted (by radius) set of StiLayers and assign a
  // corresponding padrow ID to each layer
  auto prevStiLayer = sStiLayers.begin();
  auto currStiLayer = sStiLayers.begin();

  for ( ; currStiLayer != sStiLayers.end(); ++currStiLayer)
  {
    currStiLayer->sti_padrow_id = prevStiLayer->sti_padrow_id;

    if (currStiLayer->radial_distance() != prevStiLayer->radial_distance())
      currStiLayer->sti_padrow_id++;

    prevStiLayer = currStiLayer;
  }

}
