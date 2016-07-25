/*
  root.exe 'lDb.C("sdt20140201")' RaftPosition.C+
*/
//#define CORRECT_LASER_POSITIONS
//#define CORRECT_RAFT_DIRECTION
#include "TGeoMatrix.h"
#include "StLaserAnalysisMaker/laserino.h"
#include "StLaserAnalysisMaker/LaserEvent.h"
#include "StTpcDb/StTpcDb.h"
#include "StEvent/StEnumerations.h"
#include "StarClassLibrary/StThreeVectorD.hh"
static TGeoHMatrix *Traft[14];
static TGeoHMatrix *Raft2Tpc[14];
static  const Int_t NS = 12;
static  const Int_t NB =  6;
static  const Int_t NM =  7;
static LaserB *Lasers[NS][NB][NM];
#if 1
static Int_t NoBeams = 0;
static LaserRaft *LaserBeams[NS*NB*NM];
#endif
static TGeoHMatrix *Bundles2Tpc[14][6];
static TGeoHMatrix *Mirrors2Tpc[14][6][7];
//________________________________________________________________________________
void RaftPosition(Bool_t init = kTRUE) {
  if (init) {
    StMaker::GetChain()->Init();
    StMaker::GetChain()->Make();
  }
  StTpcDb::instance()->TpcHalf(east).Print();
  StTpcDb::instance()->TpcHalf(west).Print();
  for (Int_t raft = 0; raft < NoRaftPositions; raft++) {
    RaftPositions[raft].Print();
  }
  const Int_t numSectors = 24;
  Double_t beta = 0;
  Double_t dBeta = 720./numSectors;
  Int_t sector = 0;
  TGeoHMatrix TpcHalf[2];
  Double_t rotHalfs[18] = {
    0,  1, 0, -1, 0, 0, 0, 0,  1, // sector 13-24
    0, -1, 0, -1, 0, 0, 0, 0, -1  // sector  1-12
  };
  for (Int_t half = east; half <= west; half++) TpcHalf[half].SetRotation(&rotHalfs[9*half]);
  
  Double_t zWheel     = (229.71+1.7780);
  //                      East      West
  Double_t ZWheel[2]  = {-zWheel, zWheel};
  Double_t RDWheel[2] = { 190.5802, 190.5705};
  StThreeVectorD xyzDSE(RDWheel[0], 0, ZWheel[0]);
  StThreeVectorD xyzDSW(RDWheel[1], 0, ZWheel[1]);
  TGeoHMatrix RotSec[24];
  for (sector = 1; sector <= numSectors; sector++) {
    if (sector > 12) beta = (numSectors-sector)*dBeta;
    else             beta = sector*dBeta;
    RotSec[sector-1].RotateZ(-beta);
  }
  StThreeVectorD xyzTE, xyzTW;
  TGeoHMatrix S2R[12]; // Transformation from survey line to raft
  for (sector = 1; sector <= numSectors/2; sector++) {
    cout << "Sector " <<  sector << " ===========" << endl;
    TGeoHMatrix ET = RotSec[sector+12-1].Inverse()*StTpcDb::instance()->TpcHalf(east)*RotSec[sector+12-1]; //ET.Print();
    ET.LocalToMaster(xyzDSE.xyz(), xyzTE.xyz());                             cout << "xyzTE " << xyzTE << endl;
    TGeoHMatrix EW = RotSec[sector   -1].Inverse()*StTpcDb::instance()->TpcHalf(west)*RotSec[sector   -1]; //EW.Print();
    EW.LocalToMaster(xyzDSW.xyz(), xyzTW.xyz());                             cout << "xyzTW " << xyzTW << endl;
    // Survey line direction
    StThreeVectorD dif = (xyzTW - xyzTE)/2.; cout << "dif " << dif << endl;
    // Survey line center
    StThreeVectorD sum = (xyzTW + xyzTE)/2.; cout << "sum " << sum << endl;
    StThreeVectorD unit = dif.unit();   cout << "unit " << unit << endl;
    Double_t alpha = - unit.y();
    Double_t beta  =   unit.x();
    StThreeVectorD tra = sum; tra.xyz()[0] -= 0.5*(RDWheel[0]+RDWheel[1]); cout << "tra " << tra << endl;
    // Transformation from survey line to raft
    S2R[sector-1].SetName(Form("S2R_%i",sector));
    S2R[sector-1].RotateX(alpha*180/TMath::Pi()); 
    S2R[sector-1].RotateY(beta*180/TMath::Pi());
    S2R[sector-1].SetTranslation(tra.xyz());   cout << "S2R[sector-1] "; S2R[sector-1].Print();
    StThreeVectorD unitR;
    S2R[sector-1].MasterToLocalVect(unit.xyz(),unitR.xyz()); cout << "unitR " << unitR << endl;
  }
#if 1
  memset(Traft, 0, 14*sizeof(TGeoHMatrix *));
  for (Int_t i = 0; i < NoRaftPositions; i++) {
    if (! RaftPositions[i].Sector) continue;
    Int_t raft = RaftPositions[i].Raft;
    Int_t sector = RaftPositions[i].Sector;
    if (sector > 12) sector -= 12;
    RaftPositions[i].Print();    
    TGeoHMatrix RS(Form("Raft%0i",raft)); // Raft in Survey CS
    RS.RotateX(RaftPositions[i].alpha*180/TMath::Pi());
    RS.RotateY(RaftPositions[i].beta*180/TMath::Pi());
    RS.RotateZ(RaftPositions[i].gamma*180/TMath::Pi());
    RS.SetTranslation(&RaftPositions[i].X);
    cout << "RS:"; RS.Print();
    TGeoHMatrix  T = S2R[sector-1] * RS; 
    T.SetName(Form("R%i-S%i",raft,sector));
    Traft[raft-1] = new TGeoHMatrix; 
    *Traft[raft-1] = T;
    cout << "Traft[" << raft-1 << "]:"; Traft[raft-1]->Print();
  }
#endif
}
  
