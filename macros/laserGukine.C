#include "TGeant3.h"
struct LaserTrack_t  {
  Int_t Sector;
  Int_t Mirror;
  Int_t ZBoundle;
  Int_t NoTracks; // no of laser tracks reconstracted by tpt
  Float_t psi;
  Float_t Dpsi;
  Float_t tanl;
  Float_t Dtanl;
  Float_t xl;
  Float_t Dxl;
  Float_t yl;
  Float_t Dyl;
  Float_t zl;
  Float_t Dzl;
};
LaserTrack_t LaserTracks[] = {
#include "YR4364002-3.table"
  //#include "Y2006noExB.table"
};
extern "C" void agukine_() {
  TGeant3 *geant3 = TGeant3::Geant3();
  const Double_t degrad = TMath::Pi()/180;
  Int_t NoLaserTracks = sizeof (LaserTracks)/sizeof (LaserTrack_t);
  Float_t p[3], x[3];
  Float_t pt = 1000;
  Int_t nvtx, nt;
  Int_t ntbeam = 0;
  Int_t nttarg = 0;
  Int_t Ipart = 171;
  for(Int_t i=0; i < NoLaserTracks; i++){
    if (! LaserTracks[i].NoTracks) continue;
    if (  LaserTracks[i].Sector != 12 ) continue;
    p[0] = - pt*TMath::Cos(degrad*LaserTracks[i].psi);
    p[1] = - pt*TMath::Sin(degrad*LaserTracks[i].psi);
    p[2] = - pt*LaserTracks[i].tanl;
    //Fill position vector
    x[0] = LaserTracks[i].xl;
    x[1] = LaserTracks[i].yl;
    x[2] = LaserTracks[i].zl;
    nvtx = geant3->Gsvert(x, ntbeam, nttarg, 0, 0);
    if (! nvtx) continue;
    nt = geant3->Gskine(p, Ipart, nvtx, 0, 0);
  }
}
