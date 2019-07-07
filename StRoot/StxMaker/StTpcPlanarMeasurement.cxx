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
#include "TGeoPhysicalNode.h"
#include "TGeoBBox.h"
#include "StarVMC/StarVMCApplication/StarVMCDetector.h"
#include "TMath.h"
TGeoHMatrix *StTpcPlanarMeasurement::SectorRowMatrices[24][72] = {0};
using namespace genfit;
#define PrPP(A,B) if (Debug()) {cout << "StTpcPlanarMeasurement::" << (#A) << "\t" << (#B) << " = "; (B).Print("");}
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(int nDim)
  : StPlanarMeasurement(nDim) {}
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(const TVectorD& rawHitCoords, const TMatrixDSym& rawHitCov, int detId, int hitId, TrackPoint* trackPoint)
  : StPlanarMeasurement(rawHitCoords, rawHitCov, detId, hitId, trackPoint)  {}
//________________________________________________________________________________
StTpcPlanarMeasurement::StTpcPlanarMeasurement(const StTpcHit *hit,TrackPoint* trackPoint) : StPlanarMeasurement(hit, trackPoint) {
  Int_t sector = hit->sector();
  Int_t rowRC = hit->padrow();
  Int_t NoOfInnerRows = St_tpcPadConfigC::instance()->innerPadRows(sector);
  if (NoOfInnerRows == 13) {
    if (rowRC <= NoOfInnerRows) {
      fErrCalc = StiTpcInnerHitErrorCalculator::instance(); 
    }  else {
      fErrCalc = StiTpcOuterHitErrorCalculator::instance(); 
    }
  } else {// iTPC
    if (rowRC <= NoOfInnerRows) {
      fErrCalc = StiTPCHitErrorCalculator::instance(); 
    } else {
      fErrCalc = StiTpcOuterHitErrorCalculator::instance(); 
    }
  }
  Int_t planeId = 100*sector + rowRC;
  TGeoHMatrix *DT = Plane(sector,rowRC);
  TGeoPhysicalNode *nodeP = 0;
  if (! DT) {
    TString path = hit->GetPath(); // 
#if 0
    if (! gGeoManager->CheckPath(path)) {
      cout << "Illegal path " << path.Data() << endl;
      assert(0);
    }
#endif
    TGeoPNEntry* pnEntry = gGeoManager->GetAlignableEntry(path);
    if (pnEntry) {
      nodeP = pnEntry->GetPhysicalNode();
    } else {
      cout << "TGeoPNEntry: " << path << " does not exist\n";
      pnEntry = gGeoManager->SetAlignableEntry(path, path);
      nodeP = gGeoManager->MakeAlignablePN(pnEntry);
    }
    if (! nodeP) {
      cout << "TGeoPhysicalNode with path " << path.Data() << " does not exists" << endl;
      assert(0);
    }
    const TGeoHMatrix &D = *nodeP->GetMatrix();
    //  genfit::eMeasurementType type = genfit::Spacepoint;
    //  measurementTypes.push_back(type);
    // Shift center of pad row
    Double_t shiftG[3];
    DT = new TGeoHMatrix(D);
    Double_t *r = DT->GetRotationMatrix();
    if (DT->Determinant() < 0) {
      r[1] = - r[1];
      r[4] = - r[4];
      r[7] = - r[7];
      //      Double_t shiftL[3] = {xyzL[0], 0, -((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
      Double_t shiftL[3] = {0, 0, -((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
      D.LocalToMaster(shiftL,shiftG);
    } else {
      //      Double_t shiftL[3] = {xyzL[0], 0, ((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
      Double_t shiftL[3] = {0, 0, ((TGeoBBox *)nodeP->GetVolume(-1)->GetShape())->GetDZ()};
      D.LocalToMaster(shiftL,shiftG);
    }
    DT->SetTranslation(shiftG);
    SetPlane(DT, sector, rowRC);
    if (Debug()) {
      Double_t xyzG[3]  = {fHit->position().x(), fHit->position().y(), fHit->position().z() };
      Double_t xyzGU[3] = { hit->positionU().x(), hit->positionU().y(), hit->positionU().z()};
      Double_t xyzGL[3] = { hit->positionL().x(), hit->positionL().y(), hit->positionL().z()};
      Double_t xyzL[3];
      D.MasterToLocal(xyzG, xyzL);
      Double_t xyzLT[3], xyzLTU[3], xyzLTL[3];;
      DT->MasterToLocal(xyzG,  xyzLT);
      DT->MasterToLocal(xyzGU, xyzLTU);
      DT->MasterToLocal(xyzGL, xyzLTL);
      DT->MasterToLocal(xyzG, xyzL);
      cout << path.Data() << endl;
      cout << path.Data() << " local xyz " << xyzL[0] << "/" << xyzL[1] << "/" << xyzL[2] <<  endl;
      cout << path.Data() << " local xyzT " << xyzLT[0] << "/" << xyzLT[1] << "/" << xyzLT[2] <<  endl;
      cout << path.Data() << " local xyzTU " << xyzLTU[0] << "/" << xyzLTU[1] << "/" << xyzLTU[2] <<  endl;
      cout << path.Data() << " local xyzTL " << xyzLTL[0] << "/" << xyzLTL[1] << "/" << xyzLTL[2] <<  endl;
      fHit->Print();
    }
  }
  Double_t *shiftG = DT->GetTranslation();
  TVector3 o(shiftG);
  Double_t *r = DT->GetRotationMatrix();
  TVector3 u(r[1],r[4],r[7]);
  TVector3 v(r[2],r[5],r[8]);
  genfit::DetPlane *aPlane = new genfit::DetPlane(o,u,v);
  if (Debug()) {
    aPlane->Print();
  }
  TVector3 Lab(fHit->position().x(), fHit->position().y(), fHit->position().z());
  TVector3 dist = aPlane->dist(Lab);                                              
  o -= dist;
  aPlane->setO(o);                                                              
  if (Debug()) {
    PrPP(Planenew, *aPlane);
    PrPP(Lab,Lab);
    PrPP(dist,dist);
    TVector2 OnPlane = aPlane->LabToPlane(Lab);
    PrPP(OnPlane,OnPlane);
    dist = aPlane->dist(Lab);
    PrPP(distnew,dist);
  }
  TVector2 OnPlane = aPlane->LabToPlane(Lab);
  PrPP(OnPlanenew, OnPlane);
  //  genfit::SharedPlanePtr thePlanePtr(new genfit::DetPlane(*aPlane));
  physicalPlane_.reset(aPlane);
  //  delete aPlane;
  //  setPlane(thePlanePtr,planeId);                                                  
  planeId_ = planeId;
  rawHitCoords_[0] = OnPlane.X();
  rawHitCoords_[1] = OnPlane.Y();
  Double_t ecross = 0.12*0.12, edip = 0.16*0.16;
  rawHitCov_(0,0) = ecross;
  rawHitCov_(1,1) = edip;
}
//________________________________________________________________________________
TVectorD& StTpcPlanarMeasurement::getRawHitCoords(const genfit::StateOnPlane *state) {return genfit::AbsMeasurement::getRawHitCoords(state);}
//________________________________________________________________________________
TMatrixDSym& StTpcPlanarMeasurement::getRawHitCov(const genfit::StateOnPlane *state) {
  Double_t eta  = TMath::ATan(state->getState()(1));
  Double_t tanL = state->getState()(2);
  Double_t Z    = state->getState()(4);
  TMatrixDSym &rawHitCov = genfit::AbsMeasurement::getRawHitCov(state);
  fErrCalc->calculateError(Z,eta,tanL, rawHitCov(0,0), rawHitCov(1,1));
  return rawHitCov;
}
//________________________________________________________________________________
