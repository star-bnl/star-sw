// $Id: StGeanePropagator.cxx,v 1.7 2011/02/11 16:12:52 fisyak Exp $
#include <assert.h>
#include <string.h>
#include "StGeanePropagator.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include "StarVMCApplication.h"
#include "TGeoManager.h"
TGeant3TGeo        *StGeanePropagator::fgGeant3   = 0;
Gcflag_t           *StGeanePropagator::fgcflag    = 0;
Gckine_t           *StGeanePropagator::fgckine    = 0;
Gctmed_t           *StGeanePropagator::fgctmed    = 0;
Erwork_t 	   *StGeanePropagator::fgerwork   = 0;
Ertrio_t 	   *StGeanePropagator::fgertrio   = 0;
Trcom3_t           *StGeanePropagator::fgtrcom3   = 0;
Eropts_t           *StGeanePropagator::fgeropts   = 0;
Eroptc_t           *StGeanePropagator::fgeroptc   = 0;
TGeoHMatrix        *StGeanePropagator::fRot       = 0;
StGeanePropagator  *StGeanePropagator::fgInstance = 0;
TVector3            StGeanePropagator::fxyzG      = TVector3(0,0,0);
TVector3     	    StGeanePropagator::fpxyzG     = TVector3(0,1,0);		   
Int_t        	    StGeanePropagator::fKine      = 5;
Int_t               StGeanePropagator::fDebug     = 0;
static const Double_t Delhp6 = 300;
static const Double_t Delfi6 = 0.1;
static const Double_t Cfact8 =  2.997925e-4;
//static const Double_t vL[3] = {-1, 0, 0};
static const Double_t vL[3] = { 0, 1, 0};
static const Double_t wL[3] = { 0, 0, 1};
static TGeoRotation SWAP ("SWAP",  -90.0,   90.0,   90.0,    0.0,    0.0,    0.0); // "Z180"  (x,y,z) = > ( y,-x, z)
static TGeoRotation SWAPI("SWAPI",  90.0,   90.0,  -90.0,    0.0,    0.0,    0.0); // "Z180"  (x,y,z) = > (-y, x, z)
#define PrPP(A,B) if (Debug()) {cout  << "=== StGeanePropagator::" << (#A) << "\t" << (#B) << " = \t" << ( B ) << endl;}
ClassImp(StGeanePropagator);
//________________________________________________________________________________
StGeanePropagator* StGeanePropagator::instance() {
  if (! fgInstance ) {
    StarVMCApplication::Instance();
    fgGeant3 = (TGeant3TGeo*) TVirtualMC::GetMC();
    assert(fgGeant3);
    fgcflag  = fgGeant3->Gcflag();
    fgckine  = fgGeant3->Gckine();
    fgctmed  = fgGeant3->Gctmed();
    fgerwork = fgGeant3->Erwork();
    fgertrio = fgGeant3->Ertrio();
    fgtrcom3 = fgGeant3->Trcom3();
    fgeropts = fgGeant3->Eropts();
    fgeropts->rmin =   0.0; // tracking volume 
    fgeropts->rmax = 200.0; 
    fgeropts->zmax = 210.0; 
    fgeroptc = fgGeant3->Eroptc();
    fgInstance = new StGeanePropagator();
  }
  return fgInstance;
}
//________________________________________________________________________________
void StGeanePropagator::SetMinTrackingRadius(Double_t r)      {fgeropts->rmin = r;}
void StGeanePropagator::SetMaxTrackingRadius(Double_t r)      {fgeropts->rmax = r;}
void StGeanePropagator::SetMaxTrackingZ     (Double_t z)      {fgeropts->zmax = z;}
//________________________________________________________________________________
Bool_t SensVol_t::operator==(const SensVol_t &vol) const {
  return (name == vol.name) && (ivol == vol.ivol) && (numv == vol.numv);
}
//________________________________________________________________________________
void StGeanePropagator::SetVolumes() {
  fNoSensVol = fSensVolVec.size();
  fNumv.Set(fNoSensVol);
  fIvol.Set(fNoSensVol);
  fCnamv = new Char_t*[fNoSensVol];  
  PrPP(SetVolumes, fNoSensVol);
  for (UInt_t i = 0; i < fNoSensVol; i++) {
    fCnamv[i] = (Char_t *) fSensVolVec[i].name.Data();
    fIvol[i] = fSensVolVec[i].ivol;
    fNumv[i] = fSensVolVec[i].numv;
    if (Debug()) cout << "\t" << fCnamv[i] << "/" << fIvol[i] << "/" << fNumv[i];
  }
  if (Debug()) cout << endl;
  fRot = 0;
}
//________________________________________________________________________________
void StGeanePropagator::SetSensVolume(const Char_t *name, Int_t ivol, Int_t numv) {
  UInt_t NoSensVol = fSensVolVec.size();
  SensVol_t newSensVolume(name,ivol,numv);
  for (UInt_t i = 0; i < NoSensVol; i++) {
    if (newSensVolume == fSensVolVec[i]) return;
  }
  fSensVolVec.push_back(newSensVolume);
}
//________________________________________________________________________________
Int_t StGeanePropagator::Propagate(const Char_t *opt) {
  Double_t xyzG[3], xyzL[3];
  Float_t PLANI[2][3] = {{0,1,0}, {0,0,1}};
  static Float_t xxi[3], pii[3], xxo[3], ppo[3], rd[15];
  TString Opt(opt);
  Opt += "EVZ";
  //  Int_t IfContinue = 1;
  if (! fNoSensVol) SetVolumes();
  if (! Opt.Contains("C")) {
    //    IfContinue =  0;
    TCL::ucopy(fRD.GetArray(), rd, 15);
    TGeoHMatrix *rotI = gGeoManager->GetCurrentMatrix();
    if (rotI) {
      Double_t vG[3], wG[3];
      rotI->LocalToMasterVect(vL,vG);
      rotI->LocalToMasterVect(wL,wG);
      TCL::ucopy(vG,&PLANI[0][0],3);
      TCL::ucopy(wG,&PLANI[1][0],3);
      Int_t N = 0;
      fgGeant3->Eufilp(N,rd,PLANI[0],0);
      Opt += "M";
    }
    fgGeant3->Eufilv(fNoSensVol,rd,fCnamv,fNumv.GetArray(),fIvol.GetArray());
  } else{
    fgGeant3->Eufilv(fNoSensVol, 0,fCnamv,fNumv.GetArray(),fIvol.GetArray());
  }
  GetXYZ(xyzG);
  TCL::ucopy(xyzG,xxi,3);
  GetpXYZ(pii);
  PrPP(Propagate, fxyzG);
  PrPP(Propagate, fpxyzG);
  TRSymMatrix RDI(fRD);  PrPP(Propagate, RDI);
  Double_t p = fpxyzG.Mag();
  if (p < 1e-4) return 2;
  fgGeant3->Ertrak(xxi,pii,xxo,ppo,fKine,Opt);
  if (fgertrio->ilpred != 1) return 1;
  SetXYZ(xxo);
  SetpXYZ(ppo);
  p = fpxyzG.Mag();
  if (p < 1e-4) return 2;
  SetRC(fgertrio->errout);
  fPath = gGeoManager->GetPath();
  if (Debug()) {
    cout << "arrival point " << fPath << endl;
    PrPP(Propagate, fxyzG);
    PrPP(Propagate, fpxyzG);
    PrPP(Propagate, fRC);
  }
  TRVector PDI(5,fgertrio->erpin);
  TGeoHMatrix *rotI = gGeoManager->GetCurrentMatrix();
  rotI->MasterToLocal(xyzG,xyzL);
  PDI[3] = -xyzL[0];
  PDI[4] =  xyzL[2];
  
  fRot = gGeoManager->GetCurrentMatrix();
  Double_t *tr = fRot->GetTranslation();
  TRVector vG(3), wG(3);
  fRot->LocalToMasterVect(vL,vG.GetArray());
  fRot->LocalToMasterVect(wL,wG.GetArray());
  Float_t PLANO[9];
  TCL::ucopy(vG.GetArray(),&PLANO[0],3);
  TCL::ucopy(wG.GetArray(),&PLANO[3],3);
  TCL::ucopy(&tr[0],&PLANO[6],3);
  Int_t NP = 1;
  TCL::ucopy(fRD.GetArray(), rd, 15);
  fgGeant3->Eufilp(NP,rd,PLANI[0],PLANO);
  Opt = "EMPZ";
  //  Opt = "ECPZ";
  Opt += opt;
  Opt.ReplaceAll("C","");
  GetXYZ(xxi);
  GetpXYZ(pii);
  TRVector uG = vG.Cross(wG);
  TRVector dR(3,xxi);
  dR -= TRVector(3,tr);
  TRVector dir(3,pii);
  dir = dir.Unit();
  Double_t step = (dR*uG)/(dir*uG);
  if (TMath::Abs(step) < fgctmed->prec) {
    fgGeant3->Erstor();
  } else {
    fgGeant3->Ertrak(xxi,pii,xxo,ppo,fKine,Opt);
    if (! fgertrio->ilpred) return 3;
  }
  SetXYZ(xxo);
  SetpXYZ(ppo);
  p = fpxyzG.Mag();
  if (p < 1e-7) return 2;
  SetRC(fgertrio->errout);
  
  fPath = gGeoManager->GetPath();
  PrPP(Propagate arrival point,fPath);
  PrPP(Propagate, fxyzG);
  PrPP(Propagate, fpxyzG);
  PrPP(Propagate, fRC);
  TRSymMatrix EF( 5,fgerwork->ef);        PrPP(Propagate, EF);
  fRD = TRSymMatrix(5, fgertrio->errout); PrPP(Propagate, fRD);
  fTRP = TRMatrix(5,5,fgertrio->erdtrp);  PrPP(Propagate, fTRP);
#if 0
  TRSymMatrix RDp(fTRP, TRArray::kAxSxAT, RDI); PrPP(Propagate, RDp);
#endif
  GetXYZ(xyzG);
  fRot->MasterToLocal(xyzG,xyzL);
  fPD = TRVector(6,fgertrio->erpout);
  fPD[3] = -xyzL[0];
  fPD[4] =  xyzL[2]; 
  PrPP(Propagate, fPD);
#if 0
  TRVector PDOp(fTRP,TRArray::kATxB,PDI);  
  PrPP(Propagate, PDOp);
#endif
  return 0;
}
//________________________________________________________________________________
Int_t StGeanePropagator::Propagate(const TGeoHMatrix &rotI, const TGeoHMatrix &rotO,const Char_t *opt) {
  Double_t xyzG[3], xyzL[3];

  TString chopt("EPZ");
  Float_t  rd[15];
  TCL::ucopy(fRD.GetArray(), rd, 15);
  Float_t PLANI[2][3] = {{0,1,0}, {0,0,1}};
  Double_t vG[3], wG[3];
  rotI.LocalToMasterVect(vL,vG);
  rotI.LocalToMasterVect(wL,wG);
  TCL::ucopy(vG,&PLANI[0][0],3);
  TCL::ucopy(wG,&PLANI[1][0],3);
  const Double_t *tr = rotO.GetTranslation();
  rotO.LocalToMasterVect(vL,vG);
  rotO.LocalToMasterVect(wL,wG);
  Float_t PLANO[9];
  TCL::ucopy(vG,&PLANO[0],3);
  TCL::ucopy(wG,&PLANO[3],3);
  TCL::ucopy(&tr[0],&PLANO[6],3);
  Int_t NP = 1;
  TCL::ucopy(fRD.GetArray(), rd, 15);
  fgGeant3->Eufilp(NP,rd,PLANI[0],PLANO);
  chopt = "EPZ";
  chopt += opt;
  Float_t xxi[3], pii[3], xxo[3], ppo[3];
  GetXYZ(xxi);
  GetpXYZ(pii);
  fgGeant3->Ertrak(xxi,pii,xxo,ppo,fKine,chopt);
  if (! fgertrio->ilpred) return 3;
  SetXYZ(xxo);
  SetpXYZ(ppo);
  Double_t p = fpxyzG.Mag();
  if (p < 1e-7) return 2;
  SetRC(fgertrio->errout);
  
  fPath = gGeoManager->GetPath();
  PrPP(Propagate arrival point, fPath);
  PrPP(Propagate, fxyzG);
  PrPP(Propagate, fpxyzG);
  PrPP(Propagate, fRC);
  TRSymMatrix EF( 5,fgerwork->ef); PrPP(Propagate, EF);
  fRD = TRSymMatrix(5, fgertrio->errout); PrPP(Propagate, fRD);
  fTRP = TRMatrix(5,5,fgertrio->erdtrp);  PrPP(Propagate,fTRP);
  
  GetXYZ(xyzG);
  rotO.MasterToLocal(xyzG,xyzL);
  fPD = TRVector(5,fgertrio->erpout);
  fPD[3] = -xyzL[0];
  fPD[4] =  xyzL[2]; 
  return 0;
}
//________________________________________________________________________________
Int_t StGeanePropagator::Propagate(const TGeoHMatrix &rotI, Double_t length,const Char_t *opt) {
  Float_t lengths[1] = {(Float_t) length};
  Float_t  rd[15];
  TCL::ucopy(fRD.GetArray(), rd, 15);
  Int_t NP = 1;
  fgGeant3->Eufill(NP,rd,lengths);
  TString chopt("ELZ");
  chopt += opt;
  Float_t xxi[3], pii[3], xxo[3], ppo[3];
  GetXYZ(xxi);
  GetpXYZ(pii);
  fgGeant3->Ertrak(xxi,pii,xxo,ppo,fKine,chopt);
  if (! fgertrio->ilpred) return 3;
  SetXYZ(xxo);
  SetpXYZ(ppo);
  Double_t p = fpxyzG.Mag();
  if (p < 1e-7) return 2;
  SetRC(fgertrio->errout);
  fPath = gGeoManager->GetPath();
  PrPP(Propagate arrival point, fPath);
  PrPP(Propagate, fxyzG);
  PrPP(Propagate, fpxyzG);
  PrPP(Propagate, fRC);
  TRSymMatrix EF( 5,fgerwork->ef); PrPP(Propagate, EF);
  fRD = TRSymMatrix(5, fgertrio->errout); PrPP(Propagate, fRD);
  fTRP = TRMatrix(5,5,fgertrio->erdtrp); 
  Double_t xyzG[3], xyzL[3];
  GetXYZ(xyzG);
  TGeoHMatrix *rotO = gGeoManager->GetCurrentMatrix();
  rotO->MasterToLocal(xyzG,xyzL);
  fPD = TRVector(5,fgertrio->erpout);
  fPD[3] = -xyzL[0];
  fPD[4] =  xyzL[2]; 
  return 0;
}
//________________________________________________________________________________
void StGeanePropagator::SetPD(TRVector    &PD, TGeoHMatrix *rot) {
  fPD = PD; PrPP(SetPD, PD);
  if (rot) {
    PrPP(SetPD, fxyzG);
    PrPP(SetPD, fpxyzG);
    Double_t p  = 1./PD[0];
    Double_t tY = PD[1];
    Double_t tZ = PD[2];
    TVector3 Vl(vL);
    TVector3 Wl(wL);
    TVector3 Ul = Vl.Cross(Wl);
    TVector3 PxyzL  = Ul + tY*Vl + tZ*Wl;
    PxyzL  = p*PxyzL.Unit();
    Double_t pxyzL[3];
    PxyzL.GetXYZ(pxyzL);
    Double_t pxyzG[3];
    rot->LocalToMasterVect(pxyzL, pxyzG);
    SetpXYZ(pxyzG);
    Double_t xyzL[3] = {-fPD[3], 0., fPD[4]};
    Double_t xyzG[3];
    rot->LocalToMaster(xyzL,xyzG);
    SetXYZ(xyzG);
    PrPP(SetPD, fxyzG);
    PrPP(SetPD, fpxyzG);
  }
}
// $Log: StGeanePropagator.cxx,v $
// Revision 1.7  2011/02/11 16:12:52  fisyak
// Fixes for gcc451
//
// Revision 1.6  2009/05/27 19:03:50  fisyak
// Add propagation with predefined length
//
// Revision 1.5  2009/05/26 22:00:25  fisyak
// Fix measurement directions, add step check
//
// Revision 1.4  2009/05/19 16:02:40  fisyak
// Fix derivatives
//
// Revision 1.3  2009/05/06 16:41:22  fisyak
// Add propagator between planes defined by TGeoHMatrix
//
// Revision 1.2  2009/04/29 14:37:55  fisyak
// Freeze 0-th version of VMC base reconstruction
//}

