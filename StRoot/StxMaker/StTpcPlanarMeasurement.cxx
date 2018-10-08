#include <cassert>
#include "StTpcPlanarMeasurement.h"
#include "GenFit/Exception.h"
#include "GenFit/AbsMeasurement.h"
#include "GenFit/RKTrackRep.h"
#include "GenFit/HMatrixU.h"
#include "GenFit/HMatrixV.h"
#include "GenFit/HMatrixUV.h"
#include "GenFit/StateOnPlane.h"
#include "StEvent/StTpcHit.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/StiTPCHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcInnerHitErrorCalculator.h"
#include "StDetectorDbMaker/StiTpcOuterHitErrorCalculator.h"
#include "StEvent/StTpcHit.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h"
#include "StarVMC/StarVMCApplication/StarVMCDetector.h"
using namespace genfit;
Int_t StTpcPlanarMeasurement::fDebug = 0;
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(int nDim)
  : PlanarMeasurement(nDim), fHit(0), fErrCalc(0) {}
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, TrackPoint* trackPoint)
  : PlanarMeasurement(rawHitCoords, rawHitCov, detId, hitId, trackPoint), fHit(0), fErrCalc(0) {}
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(const StTpcHit *hit,TrackPoint* trackPoint) : PlanarMeasurement(2), fHit(hit), fErrCalc(0) {
  detId_ = (Int_t) fHit->detector();
  hitId_ = fHit->id();
  Int_t sector = fHit->sector();
  Int_t half   = (sector  - 1)/12 + 1;
  Int_t sectorVMC = (sector - 1)%12 + 1;
  Int_t rowRC = fHit->padrow();
  Int_t rowVMC = 0;
  Int_t NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
  Int_t NoOfRows = St_tpcPadConfigC::instance()->padRows(sector);
  fErrCalc = 0;
  if (NoOfInnerRows == 13) {
    if (rowRC <= NoOfInnerRows) {rowVMC = 3*(rowRC -  1) +  2;  fErrCalc = StiTpcInnerHitErrorCalculator::instance(); }
    else                        {rowVMC =   (rowRC - 14  + 41); fErrCalc = StiTpcOuterHitErrorCalculator::instance(); }
    if (rowVMC > 72)   rowVMC = 72;
  } else {// iTPC
    if (rowRC <= NoOfInnerRows) {
      rowVMC = rowRC + 1; 
      if (rowVMC <  2) rowVMC =  2; 
      if (rowVMC > 41) rowVMC = 41;
      fErrCalc = StiTPCHitErrorCalculator::instance(); 
    } else {
      rowVMC = rowRC + 3;
      if (rowVMC < 44) rowVMC = 44;
      if (rowRC > NoOfRows) rowRC = NoOfRows;
      fErrCalc = StiTpcOuterHitErrorCalculator::instance(); 
    }
  }
  Int_t planeId = 100*sector + rowRC;
  Int_t indx[3] = {half, sectorVMC, rowVMC};
  static TString path2TPC("/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1/TPGV_%d/TPSS_%d/TPAD_%d");
  TString path(StarVMCDetector::FormPath(path2TPC,3,indx));
  if (! gGeoManager->CheckPath(path)) {
    cout << "Illegal path " << path.Data() << endl;
    assert(0);
  }
  TGeoPhysicalNode *nodeP = gGeoManager->MakePhysicalNode(path);
  if (! nodeP) {
    cout << "TGeoPhysicalNode with path " << path.Data() << " does not exists" << endl;
    assert(0);
  }
  const TGeoHMatrix &D = *nodeP->GetMatrix();
  //  genfit::eMeasurementType type = genfit::Spacepoint;
  //  measurementTypes.push_back(type);
  Double_t xyzG[3] = {fHit->position().x(),fHit->position().y(),fHit->position().z()};
  Double_t xyzL[3];
  D.MasterToLocal(xyzG, xyzL);
  // Shift center of pad row
  Double_t shiftG[3];
  TGeoHMatrix DT(D);
  if (DT.Determinant() < 0) {
    Double_t *r = DT.GetRotationMatrix();
    //      for (Int_t i = 0; i < 9; i++) r[i] = - r[i];
    r[1] = - r[1];
    r[4] = - r[4];
    r[7] = - r[7];
    Double_t shiftL[3] = {xyzL[0], 0, -((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
    D.LocalToMaster(shiftL,shiftG);
  } else {
    Double_t shiftL[3] = {xyzL[0], 0, ((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
    D.LocalToMaster(shiftL,shiftG);
  }
  DT.SetTranslation(shiftG);
  Double_t xyzLT[3];
  DT.MasterToLocal(xyzG, xyzLT);
#if 0
  Double_t PosG[3] = {posSeed.x(), posSeed.y(), posSeed.z()}; 
  Double_t PosL[3];
  DT.MasterToLocal(PosG,PosL);
#endif
  TVector3 o(shiftG);
  TVector3 u(DT.GetRotationMatrix()+3);
  TVector3 v(DT.GetRotationMatrix()+6);
  DT.MasterToLocal(xyzG, xyzL);
  setPlane(genfit::SharedPlanePtr(new genfit::DetPlane(o,u,v)),planeId);
  rawHitCoords_[0] = xyzL[1];
  rawHitCoords_[1] = xyzL[2];
  Double_t ecross = 0.12*0.12, edip = 0.16*0.16;
#if 0
  fErrCalc->calculateError(xyzL[2], pars.eta(), pars.tanl(), ecross, edip);
#endif
  rawHitCov_(0,0) = ecross;
  rawHitCov_(1,1) = edip;
  if (Debug()) {
    cout << path.Data() << " local xyz " << xyzL[0] << "/" << xyzL[1] << "/" << xyzL[2] <<  endl;
    cout << path.Data() << " local xyzT " << xyzLT[0] << "/" << xyzLT[1] << "/" << xyzLT[2] <<  endl;
    fHit->Print();
  }
}
//________________________________________________________________________________
const TVectorD& StTpcPlanarMeasurement::getRawHitCoords(genfit::StateOnPlane *state) const {return genfit::AbsMeasurement::getRawHitCoords(state);}
//________________________________________________________________________________
const TMatrixDSym& StTpcPlanarMeasurement::getRawHitCov(genfit::StateOnPlane *state) const {return genfit::AbsMeasurement::getRawHitCov(state);}
//________________________________________________________________________________
TVectorD& StTpcPlanarMeasurement::getRawHitCoords(genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCoords(state);}
//________________________________________________________________________________
TMatrixDSym& StTpcPlanarMeasurement::getRawHitCov(genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCov(state);}
//________________________________________________________________________________
