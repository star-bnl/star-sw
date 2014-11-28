#include "Stiostream.h"
#include "FitParameters.h"
#include "TVector3.h"
#include "TGeoMatrix.h"
#include "StarMagField.h"
#include "TGeant3.h"
/*
  TRPTSC(PC,RC,PD,RD,IERR):                (1/Pt,LAMBDA,PHI,YT,ZT) => (1/P,LAMBDA,PHI,YT,ZT)
  TRSCPT(PC,RC,PD,RD,IERR):                (1/P,LAMBDA,PHI,YT,ZT)  => (1/Pt,LAMBDA,PHI,YT,ZT)

  TRSDPT(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK): (1/P,V',W',V,W)         => (1/Pt,V',W',V,W)
  TRPTSD(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK): (1/Pt,V',W',V,W)        => (1/P, V',W',V,W)

  TRS1S2 (PD1,RD1,PD2,RD2,H,CH,IERR,SP1,SP2,DJ1,DK1,DJ2,DK2)
  :                                        (1/P,V1',W1',V1,W1)     => (1/P,V2',W2',V2,W2)

> TRSCSD(PC,RC,PD,RD,H,CH,IERR,SPU,DJ,DK): (1/P,LAMBDA,PHI,YT,ZT)  => (1/P,V',W',V,W)
> TRSDSC(PD,RD,PC,RC,H,CH,IERR,SPU,DJ,DK): (1/P,V',W',V,W)         => (1/P,LAMBDA,PHI,YT,ZT)

> TRSCSP(PC,RC,PS,RS,H,CH,IERR,SPX):       (1/P,LAMBDA,PHI,YT,ZT)  => (1/P,Y',Z',Y,Z)
> TRSPSC(PS,RS,PC,RC,H,CH,IERR,SPX):       (1/P,Y',Z',Y,Z)         => (1/P,LAMBDA,PHI,YT,ZT)

   virtual void Trscsd(Float_t *pc,Float_t *rc,Float_t *pd,Float_t *rd,Float_t *h,
			Float_t &ch,Int_t &ierr,Float_t &spu,Float_t *dj,Float_t *dk);
   virtual void Trsdsc(Float_t *pd,Float_t *rd,Float_t *pc,Float_t *rc,Float_t *h,
			Float_t &ch,Int_t &ierr,Float_t &spu,Float_t *dj,Float_t *dk);
   virtual void Trscsp(Float_t *ps,Float_t *rs,Float_t *pc,Float_t *rc,Float_t *h,
			Float_t &ch,Int_t &ierr,Float_t &spx);
   virtual void Trspsc(Float_t *ps,Float_t *rs,Float_t *pc,Float_t *rc,Float_t *h,
			Float_t &ch,Int_t &ierr,Float_t &spx);
 */
TRMatrix **FitParameters::_H = 0;
TRMatrix FitParameters::_F = TRMatrix(TRArray::kUnit,5);
Int_t FitParameters::_debug = 0;
static const Double_t vL[3] = { 0, 1, 0};
static const Double_t wL[3] = { 0, 0, 1};
#define PrPC(A,B) {if (_debug & 16) {cout  << "=== FitParameters::" << (#A) << "\t" << (#B) << " = \t" << ( B ) << endl;}}
//________________________________________________________________________________
ostream& operator<<(ostream& s,const FitParameters &target) {
  s << "type = " << target.Type();
  switch (target.Type()) {
  case FitParameters::kSD:   s << "\tSD"; break;
  case FitParameters::kDca:  s << "\tDca"; break;
  case FitParameters::kSC:   s << "\tSC";   break;
  case FitParameters::kSti:  s << "\tSti";  break;
  default:                   s << "\tUnknown"; break;
  }
  s << endl;
  s << "parameters\t" << target.P() << "\tspu\t" << target.Spu() << endl;
  s << "cov.matrix\t" << target.C() << endl;
  if (target.Rot())
    s << "rot.matrix\t" << target.Rot()->GetName() << endl;
  s << "BField\t" << target.BField();
  return s;
 }
//________________________________________________________________________________
FitParameters::eTrParType FitParameters::SetType(Int_t type){
  FitParameters::eTrParType _t = kUndefined;
  switch (type) {
  case kSD:   _t = kSD;   break;
  case kSC:   _t = kSC;   break;
  case kSti:  _t = kSti;  break;
  case kDca:  _t = kDca;  break;
  default: break;
  }
  return _t;
}
//________________________________________________________________________________
void FitParameters::Setup(const TRVector *P, const TRSymMatrix *C) { 
  memset(_beg, 0, _end-_beg);
  _P.AdoptA(0,_par); 
  _C.AdoptA(0,_cov);
  _BField.AdoptA(3,_bfield);
  if (P) {_P = *P; 
    if (_type != kUndefined) {
      StarMagField::Instance()->BField(XyzG().GetArray(),_bfield);
    }
  }
  if (C) _C = *C;
  if (! _H) {_H = new TRMatrix*[kAll]; memset(_H, 0, kAll*sizeof(TRMatrix*));}
  if (! _H[_type]) {
    switch (_type) {
    case kSD :  _H[_type] = new TRMatrix(2,5,//q/p tY tZ, y, z
					 0., 0., 0., 1., 0., // y
					 0., 0., 0., 0., 1.);// z
      break;
    case kDca:  assert(0);
    case kSC :  _H[_type] = new TRMatrix(TRArray::kUnit,5); break;
    case kSti:  _H[_type] = new TRMatrix(2,6, 
					 //x y   z Phi 1/pT  tanl
					 0., 1., 0., 0., 0., 0., // y
					 0., 0., 1., 0., 0., 0.);// z
      break;
    default: assert(0);
    }
  }
}
//________________________________________________________________________________
TRVector &FitParameters::PxyzG() const {		
  static TRVector hold;
  static FitParameters *Old;
  if (Old == this) return *&hold;
  Old = (FitParameters *) this;
  switch (_type) {				
  case kSD :  hold = PxyzGSD (); break;	
  case kDca:  hold = PxyzGDca(); break;	
  case kSC :  hold = PxyzGSC (); break;	
  case kSti:  hold = PxyzGSti(); break;	
  default:;				
  }			
  return *&hold;
}
//________________________________________________________________________________
TRSymMatrix &FitParameters::PxyzGCovMatrix() const {		
  static TRSymMatrix hold;
  static FitParameters *Old;
  if (Old == this) return *&hold;
  Old = (FitParameters *) this;
  switch (_type) {				
  case kSD:   hold = PxyzGCovMatrixSD (); break;	
  case kDca:  hold = PxyzGCovMatrixDca(); break;	
  case kSC:   hold = PxyzGCovMatrixSC (); break;	
  case kSti:  hold = PxyzGCovMatrixSti(); break;	
  default:;				
  }					
  return *&hold;	
}
//________________________________________________________________________________
TRVector &FitParameters::XyzG() const {		
  static TRVector hold;
  static FitParameters *Old;
  if (Old == this) return *&hold;
  Old = (FitParameters *) this;
  switch (_type) {				
  case kSD:   hold = XyzGSD (); break;	
  case kDca:  hold = XyzGDca(); break;	
  case kSC:   hold = XyzGSC (); break;	
  case kSti:  hold = XyzGSti(); break;	
  default:;
  }						
  return *&hold;	
}
//________________________________________________________________________________
TRSymMatrix &FitParameters::XyzGCovMatrix() const {		
  static TRSymMatrix hold;
  static FitParameters *Old;
  if (Old == this) return *&hold;
  Old = (FitParameters *) this;
  switch (_type) {				
  case kSD:   hold = XyzGCovMatrixSD (); break;	
  case kDca:  hold = XyzGCovMatrixDca(); break;	
  case kSC:   hold = XyzGCovMatrixSC (); break;	
  case kSti:  hold = XyzGCovMatrixSti(); break;	
  default:;
  }						
  return *&hold;	
}
//________________________________________________________________________________
void FitParameters::Momentum(Double_t p[3], Double_t e[6]) const {
  TCL::ucopy(PxyzG().GetArray(), p, 3);
  if (!e) return;
  TCL::ucopy (PxyzGCovMatrix().GetArray(), e, 6);
}
//________________________________________________________________________________
Int_t FitParameters::Charge() const {
  switch (_type) {
  case kSC:   return (Int_t)   TMath::Sign(1., P()[0]); // pInv
  case kSD:   return (Int_t)   TMath::Sign(1., P()[0]); // pInv
  case kDca:  return (Int_t)   TMath::Sign(1., P()[3]); // pTInv
  case kSti:  return (Int_t)  -TMath::Sign(1., P()[4]); // ptin = -q/pT
  default:                   assert(0);
  }
  return 0;
}
//________________________________________________________________________________
Double_t FitParameters::Y() const { // correspond to measurement in bending plane 
  switch (_type) {
  case kSD:   return P()[3]; // y
  case kSti:  return P()[1]; // 
  default:                   assert(0);
  }
  return 0;
}
//________________________________________________________________________________
Double_t FitParameters::Z() const { //  correspond to measurement in non bending plane 
  switch (_type) {
  case kSD:   return P()[4]; // z
  case kSti:  return P()[2]; //
  default:                   assert(0);
  }
  return 0;
}
//________________________________________________________________________________
Double_t FitParameters::Eta() const { //  Phi angle wrt local X axis
  if (_type == kSD) {return TMath::ATan(P()[1]);}
  assert(_rot);
  Double_t dirL[3];
  _rot->MasterToLocalVect(PxyzG().GetArray(), dirL);
  return TMath::ATan2(dirL[1],dirL[0]);
}
//________________________________________________________________________________
Double_t FitParameters::TanL() const { //  
  if (_type == kSD) {return P()[2];}
  TVector3 p3(PxyzG().GetArray());
  return p3.Pz()/p3.Perp();
}
//________________________________________________________________________________
Double_t FitParameters::pT() const { //  
  TVector3 p3(PxyzG().GetArray());
  return p3.Perp();
}
//________________________________________________________________________________
Double_t FitParameters::dpTOverpT() const { //  
  Int_t k = 0;
  switch (_type) {
  case kSD:   k = 0; break;
  case kSC:   k = 0; break;
  case kDca:  k = 3; break;
  case kSti:  k = 4; break;
  default:       assert(0);
  }
  Double_t p     = P()(k);
  Double_t cpIpI = C()(k,k);
  if (cpIpI <= 0.) return -99.;
  return TMath::Sqrt(cpIpI*p*p);
}
//________________________________________________________________________________
FitParameters   &FitParameters::operator=(const    FitParameters &source) {
  if (this != &source) { 
    if (_type ==  FitParameters::kUndefined || _type == source.Type()) {
      _type = source.Type(); _spu = source.Spu(); 
      _spu  = source.Spu();
      //      memcpy(_beg, source.beg(), _end - _beg);
      _P      = source.P();
      _C      = source.C();
      _BField = source.BField();
      _rot = (TGeoHMatrix *) source.Rot();
      _F  = TRMatrix(TRArray::kUnit,5);
    } else {
      StarMagField::Instance()->BField(source.XyzG().GetArray(),_bfield);
      switch (source.Type()) {
      case kSD:  
	switch (_type) {
	case kSC:   *this = SD2SC (source); break;
	case kDca:  *this = SD2Dca(source); break;
	case kSti:  *this = SD2Sti(source); break;
	default:       assert(0);
	}
	break;
      case kSC:
	switch (_type) {
	case kSD:  *this = SC2SD (source); break;
	case kDca: *this = SC2Dca(source); break;
	case kSti: *this = SC2Sti(source); break;
	default:       assert(0);
	}
	break;
      case kDca:
	switch (_type) {
	case kSD:  *this = Dca2SD (source); break;
	case kSC:  *this = Dca2SC (source); break; 
	case kSti: *this = Dca2Sti(source); break;
	default:       assert(0);
	}
	break;
      case kSti:
	switch (_type) {
	case kSD:  *this =Sti2SD (source); break;   
	case kSC:  *this =Sti2SC (source); break;    
	case kDca: *this =Sti2Dca(source); break;    
	default:       assert(0);
	}
	break;
      default:         assert(0);
      }
    }
  }
  return *this;
}
//______________________  SC  ___________________________________________________
//________________________________________________________________________________
TRVector  FitParameters::PxyzGSC() const {
  //           pars =>  (pInv=q/p, Lambda, Phi, yT, zT, xT
  ParamSC_t *pars = (ParamSC_t *) P().GetArray();
  Double_t p = (TMath::Abs(pars->pInv)<1e-6) ? 1e6: 1./TMath::Abs(pars->pInv);
  Double_t pxyz[3] = {p*TMath::Cos(pars->Phi)*TMath::Cos(pars->Lambda), 
		      p*TMath::Sin(pars->Phi)*TMath::Cos(pars->Lambda),  
		      p*                      TMath::Sin(pars->Lambda)};
  return TRVector(3,pxyz);
}
//________________________________________________________________________________
TRSymMatrix  FitParameters::PxyzGCovMatrixSC() const {
  //           pars =>  (pInv=q/p, Lambda, Phi, yT, zT, xT)
  ParamSC_t *pars = (ParamSC_t *) P().GetArray();
  Double_t q = TMath::Sign(1.,pars->pInv);
  Double_t p = (TMath::Abs(pars->pInv)<1e-6) ? 1e6: q/pars->pInv;
  Double_t qp2 = q*p*p;
  Double_t cosP = TMath::Cos(pars->Phi);
  Double_t sinP = TMath::Sin(pars->Phi);
  Double_t cosL = TMath::Cos(pars->Lambda);
  Double_t sinL = TMath::Sin(pars->Lambda);
  Double_t dPxyzdSC[15] = {// I have not check 
    //    pInv=q/p,       Lambda,          Phi, yT, zT
    -qp2*cosP*cosL, -p*cosP*sinL, -p*sinP*cosL, 0., 0.,  // px
    -qp2*sinP*cosL, -p*sinP*sinL,  p*cosP*cosL, 0., 0.,  // py
    -qp2*     sinL,  p*     cosL,           0., 0., 0.}; // pz
  TRMatrix F(3,6,dPxyzdSC);
  return TRSymMatrix(F,TRArray::kAxSxAT,C());
}
//________________________________________________________________________________
TRVector  FitParameters::XyzGSC() const {
  //        pars =>  (pInv=q/p, Lambda, Phi, yT, zT, xT)   
  ParamSC_t *pars = (ParamSC_t *) P().GetArray();
  Double_t xyzG[3] = {pars->xT, pars->yT, pars->zT};
  return TRVector(3,xyzG);
}
//________________________________________________________________________________
TRSymMatrix  FitParameters::XyzGCovMatrixSC() const {
  //        pars =>  (pInv=q/p, Lambda, Phi, yT, zT, xT)   
  ParamSC_t *pars = (ParamSC_t *) P().GetArray();
  Double_t cosP = TMath::Cos(pars->Phi);
  Double_t sinP = TMath::Sin(pars->Phi);
  Double_t cosL = TMath::Cos(pars->Lambda);
  Double_t sinL = TMath::Sin(pars->Lambda);
  Double_t dXyzGdSC[15] = {
    //      (cosP*cosL, -sinP, cosP*sinL)    (0 )    (x)
    // R =  (sinP*cosL,  cosP, sinP*sinL) *  (yT) => (y)
    //      (     sinL,     0,      cosL)    (zT)    (z)
    // x = - sinP*yT + cosP*sinL*zT
    // y =   cosP*yT + sinP*sinL*zT
    // z =                  cosL*zY
    //    pInv=q/p,Lambda,Phi,    yT,        zT
    0             ,     0,  0, -sinP, cosP*sinL,   // x
    0             ,     0,  0,  cosP, sinP*sinL,   // y
    0             ,     0,  0,     0,      cosL};  // z
  TRMatrix R(3,5,dXyzGdSC);
  return TRSymMatrix(R,TRArray::kAxSxAT,C());
}
//________________________________________________________________________________
FitParameters  &FitParameters::SD2SC(const FitParameters &source) {
  if (this != &source) {
    if (! _rot) _rot = (TGeoHMatrix *) source.Rot();
    assert(_rot);
    Float_t pd[5];  TCL::ucopy(source.P().GetArray(), pd, 5);
    Float_t rd[15]; TCL::ucopy(source.C().GetArray(), rd, 15);
    Float_t h[3];   TCL::ucopy(source.BField().GetArray(), h, 3);
    Float_t spu = source.Spu();
    TRVector vG(3), wG(3);
    _rot->LocalToMasterVect(vL,vG.GetArray());
    _rot->LocalToMasterVect(wL,wG.GetArray());
    Float_t dj[3]; TCL::ucopy(vG.GetArray(), dj, 3);
    Float_t dk[3]; TCL::ucopy(wG.GetArray(), dk, 3);
    Float_t ch = source.Charge();
    Float_t pc[5], rc[15];
    Int_t ierr;
    ((TGeant3 *) gMC)->Trsdsc(pd,rd,pc,rc,h,ch,ierr,spu,dj,dk);
    assert(!ierr);
    pc[0] *= ch; // 1/p => q/p
    P() = TRVector(5,pc);
    C() = TRSymMatrix(5,rc);
    F() = TRMatrix(5,5,((TGeant3 *) gMC)->Ertrio()->erdtrp); 
  }
  return *this;
}
//________________________________________________________________________________
FitParameters  &FitParameters::Dca2SC(const FitParameters &source) {
  if (this != &source) {
    assert(0);
  }
  return *this;
}
//________________________________________________________________________________
FitParameters  &FitParameters::Dca2SD(const FitParameters &source) {
  if (this != &source) {
    assert(0);
  }
  return *this;
}
//________________________________________________________________________________
FitParameters  &FitParameters::Sti2SC(const FitParameters &source) {
  if ((FitParameters *) this != (FitParameters *)&source) {
    assert(0);
  }
  return *this;
}
//______________________  SD  ___________________________________________________
//________________________________________________________________________________
TRVector  FitParameters::PxyzGSD() const {
  //       pars =>  pInv, tY, tZ, y, z; spu => track direction projection on local x axis// on a plane, pInv = q/p; 
  assert(_rot);
  assert(_spu);
  ParamSD_t *pars = (ParamSD_t *) P().GetArray();
  Double_t p = (TMath::Abs(pars->pInv)<1e-6) ? 1e6: 1./TMath::Abs(pars->pInv);
  Double_t norm = _spu*p/TMath::Sqrt(1. + pars->tY*pars->tY + pars->tZ*pars->tZ);
  Double_t pxyzL[3] = {norm, norm*pars->tY, norm*pars->tZ};
  Double_t pxyzG[3];
  _rot->LocalToMasterVect(pxyzL,pxyzG);
  return TRVector(3,pxyzG);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::PxyzGCovMatrixSD() const {
  //       pars =>  pInv, tY, tZ, y, z; spu => track direction projection on local x axis// on a plane, pInv = q/p; 
  assert(0);
  return TRSymMatrix(3);
}
//________________________________________________________________________________
TRVector  FitParameters::XyzGSD() const {
  assert(_rot);
  //       pars =>  pInv, tY, tZ, y, z; spu => track direction projection on local x axis// on a plane, pInv = q/p; 
  ParamSD_t *pars = (ParamSD_t *) P().GetArray();
  Double_t xyzL[3] = { 0, pars->y, pars->z};
  Double_t xyzG[3];
  _rot->LocalToMaster(xyzL,xyzG);
  return TRVector(3,xyzG);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::XyzGCovMatrixSD() const {
  assert(0);
  //       pars =>  pInv, tY, tZ, y, z; spu => track direction projection on local x axis// on a plane, pInv = q/p; 
  //  ParamSD_t *pars = (ParamSD_t *) P().GetArray();
  return TRSymMatrix(3);
}
//________________________________________________________________________________
FitParameters  &FitParameters::SC2SD(const FitParameters &source) {
  if (this != &source) {
    if (! _rot) _rot = (TGeoHMatrix *) source.Rot();
    assert(_rot);
    Float_t pc[5];  TCL::ucopy(source.P().GetArray(), pc, 5);
    Float_t rc[15]; TCL::ucopy(source.C().GetArray(), rc, 15);
    Float_t h[3];   TCL::ucopy(source.BField().GetArray(), h, 3);
    TRVector vG(3), wG(3);
    _rot->LocalToMasterVect(vL,vG.GetArray());
    _rot->LocalToMasterVect(wL,wG.GetArray());
    Float_t dj[3]; TCL::ucopy(vG.GetArray(), dj, 3);
    Float_t dk[3]; TCL::ucopy(wG.GetArray(), dk, 3);
    Float_t ch = source.Charge();
    Float_t pd[5], rd[15];
    Int_t ierr;
    Float_t spu;
    ((TGeant3 *) gMC)->Trscsd(pc,rc,pd,rd,h,ch,ierr,spu,dj,dk);
    pd[0] *= ch;
    const ParamSC_t *parSC = (const ParamSC_t *) source.P().GetArray();
    Double_t xyzG[3] = {parSC->xT, parSC->yT, parSC->zT};
    Double_t xyzL[3];
    _rot->MasterToLocal(xyzG,xyzL);
    P() = TRVector(5,pd);
    ParamSD_t *parSD = (ParamSD_t *) P().GetArray();
    parSD->y = xyzL[1];
    parSD->z = xyzL[2];
    C() = TRSymMatrix(5,rd);
    Spu() = spu;
    assert(!ierr);
    F() = TRMatrix(5,5,((TGeant3 *) gMC)->Ertrio()->erdtrp); 
  }
  return *this;
}
//________________________________________________________________________________
FitParameters &FitParameters::Sti2SD(const FitParameters &source) {// LoadS2D
  if (this != &source) {
    // source = {_y, _z, _eta, _ptin, _tanl}  eta == Psi;
    _rot = (TGeoHMatrix *) source.Rot();
    assert(_rot);
    ParamSti_t *pars = (ParamSti_t *) source.P().GetArray();
    Double_t xyzL[3] = {pars->x, pars->y, pars->z};
    Double_t xyzG[3];
    _rot->LocalToMaster(xyzL,xyzG);
    Double_t q = -TMath::Sign(1.,pars->ptin);
    Double_t pT = (TMath::Abs(pars->ptin)<1e-6) ? 1e6: 1./TMath::Abs(pars->ptin);
    Double_t Phi = pars->eta;
    Double_t cosPhi = TMath::Cos(Phi);
    Double_t sinPhi = TMath::Sin(Phi);
    Double_t pxyzG[3] = {pT*cosPhi, pT*sinPhi, pT*pars->tanl};
    TVector3 PxyzG(pxyzG);
    Double_t p = PxyzG.Mag();
    Double_t cosL = 1./TMath::Sqrt(1. + pars->tanl*pars->tanl);
    Double_t cosL2 = cosL*cosL;
    Double_t cosL3 = cosL2*cosL;
    Double_t sinL = cosL*pars->tanl;
    TRVector nG     (3, cosPhi*cosL, sinPhi*cosL, sinL); PrPC(Sti2SD,nG);
    //   d(tY,tZ)          d(tY,tZ)       d(nL_x,nL_y,nL_y)   d(nG_x,nG_y,nG_y)
    // ----------- =  ----------------- x ----------------- x -----------------
    // d(Phi,tanl)    d(nL_x,nL_y,nL_y)   d(nG_x,nG_y,nG_y)      d(Phi,tanl)
    //
    //             = dtYtZdnL           x ROT**-1           x dnGdPhitanl
    TRVector nL(3);
    _rot->MasterToLocalVect(nG.GetArray(),nL.GetArray()); PrPC(Sti2SD,nL);
    //                              v' = tY      w' = tZ            Y        Z
    TRVector PD = TRVector(5, q/p, nL[1]/nL[0], nL[2]/nL[0], xyzL[1], xyzL[2]); PrPC(Sti2SD,PD);
    TRMatrix   dtYtZdnL(2,3,//nx ny  nz 
			-PD[1], 1., 0.,  // tY 
			-PD[2], 0., 1.); // tZ
    dtYtZdnL /= nL[0];
    PrPC(Sti2SD,dtYtZdnL);
    TRMatrix   dnGdPhitanl(3,2,//    Phi              tanL
			   -sinPhi*cosL, -sinL*cosL2*cosPhi,     // nx
			   cosPhi*cosL , -sinL*cosL2*sinPhi,     // ny
			   0.          ,             cosL3);     // nz
    PrPC(Sti2SD,dnGdPhitanl);
    TGeoHMatrix ROTI = _rot->Inverse(); // G => L
    TRMatrix dnLdnG(3,3,ROTI.GetRotationMatrix()); // G => L
    PrPC(Sti2SD,dnLdnG);
    TRMatrix dnLdPhitanl(dnLdnG,TRArray::kAxB,dnGdPhitanl);
    PrPC(Sti2SD,dnLdPhitanl);
    TRMatrix dtYtZdPhitanl(dtYtZdnL,TRArray::kAxB,dnLdPhitanl);
    PrPC(Sti2SD,dtYtZdPhitanl);
    Double_t dPDdPS[25] = { 
      //y z                Phi  -q/pT               tanL
      0, 0,                  0,-q*cosL, -q/pT*cosL2*sinL, // 1/p 
      0, 0, dtYtZdPhitanl(0,0),     0, dtYtZdPhitanl(0,1), // v' = tY
      0, 0, dtYtZdPhitanl(1,0),     0, dtYtZdPhitanl(1,1), // w' = tZ
      1, 0,                  0,     0,                  0, // v = y
      0, 1,                  0,     0,                  0};// w = z
    F() = TRMatrix(5,5,dPDdPS); // d(1/p,tY,tZ,y,z)/d(y,z,Phi,1/pT,tanl)
    PrPC(Sti2SD,F());
    TRSymMatrix RS6(6, source.C().GetArray()); PrPC(Sti2SD,RS6);
    TRSymMatrix RS(5, 
		   RS6(1,1),
		   RS6(1,2), RS6(2,2),
		   RS6(1,3), RS6(2,3), RS6(3,3),
		   RS6(1,4), RS6(2,4), RS6(3,4), RS6(4,4),
		   RS6(1,5), RS6(2,5), RS6(3,5), RS6(4,5), RS6(5,5)); PrPC(Sti2SD,RS);
    TRSymMatrix RD(F(), TRArray::kAxSxAT,RS); PrPC(Sti2SD,RD);
    _spu = TMath::Sign(1.0, nL[0]);
    P() = PD;
    C() = RD;
  }
  return *this;
}
//______________________  Sti ___________________________________________________
//________________________________________________________________________________
TRVector FitParameters::PxyzGSti() const {
  //           pars =>  (x_fixed,y,z,eta==Phi,ptin=-q/pT,tanl)
  ParamSti_t *pars = (ParamSti_t *) P().GetArray();
  Double_t pT = (TMath::Abs(pars->ptin)<1e-6) ? 1e6: 1./TMath::Abs(pars->ptin);
  Double_t pxyz[3] = {pT*TMath::Cos(pars->eta), pT*TMath::Sin(pars->eta),  pT*pars->tanl};
  return TRVector(3,pxyz);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::PxyzGCovMatrixSti() const {
  //           pars =>  (x_fixed,y,z,eta==Phi,ptin=-q/pT,tanl)
  ParamSti_t *pars = (ParamSti_t *) P().GetArray();
  Double_t pT = (TMath::Abs(pars->ptin)<1e-6) ? 1e6: 1./TMath::Abs(pars->ptin);
  Double_t dPTdPi = -pT*pT; if (pars->ptin < 0 ) dPTdPi=-dPTdPi;
  Double_t dPxyzdSti[18] = {// I have not check 
    //x,y,z  eta                         ptin                    tanl
    0, 0, 0, -pT    *TMath::Sin(pars->eta), dPTdPi*TMath::Cos(pars->eta), 0,   // px
    0, 0, 0,  pT    *TMath::Cos(pars->eta), dPTdPi*TMath::Sin(pars->eta), 0,   // py
    0, 0, 0,                             0, dPTdPi*pars->tanl           , pT}; // pz
  TRMatrix F(3,6,dPxyzdSti);
  return TRSymMatrix(F,TRArray::kAxSxAT,C());
}
//________________________________________________________________________________
TRVector FitParameters::XyzGSti() const {
  //           pars =>  (x_fixed,y,z,eta==Phi,ptin=-q/pT,tanl)
  assert(_rot);
  ParamSti_t *pars = (ParamSti_t *) P().GetArray();
  Double_t  *xyzL = &pars->x;
  Double_t xyzG[3];
  _rot->LocalToMaster(xyzL,xyzG);
  return TRVector(3,xyzG);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::XyzGCovMatrixSti() const {
  //           pars =>  (x_fixed,y,z,eta==Phi,ptin=-q/pT,tanl)
  assert(_rot);
  TRMatrix Rot(3,3,_rot->GetRotationMatrix()); // L->G
  TRSymMatrix C3x3(3,C().GetArray()); // coordinate part of cov. matrix
  return TRSymMatrix(Rot,TRArray::kAxSxAT,C3x3);
}
//________________________________________________________________________________
FitParameters &FitParameters::SC2Sti(const FitParameters &source) {
  if (this != &source) {
    FitParameters SD(kSD); 
    SD = source;
    *this = SD;
  }
  return *this;
}
//________________________________________________________________________________
FitParameters &FitParameters::SD2Sti(const FitParameters &source) { // LoadD2S
  if (this != &source) {
    _rot = (TGeoHMatrix *) source.Rot();
    assert(_rot);
    // SD: pInv, tY, tZ, y, z; // on a plane
    const ParamSD_t *parsSD = (const ParamSD_t *) source.P().GetArray();
    // Prediction = {_y, _z, _eta, _ptin, _tanl}  eta == Psi;
    TVector3 pxyz(source.PxyzG().GetArray()); PrPC(SD2Sti,source.P());
    Double_t p = pxyz.Mag();
    Double_t q = source.Charge();
    Double_t Phi = pxyz.Phi();
    Double_t TanL = 1./TMath::Tan(pxyz.Theta());
    Double_t pT = pxyz.Perp();
    //                   x,         y,         z, Psi, ptin, TanL
    Double_t pSti[6] = {0., parsSD->y, parsSD->z, Phi, -q/pT, TanL};
    P() = TRVector(6,pSti);                   PrPC(SD2Sti,P());
    const ParamSti_t *pars = (const ParamSti_t *) P().GetArray();
    Double_t cosPhi = TMath::Cos(Phi);
    Double_t sinPhi = TMath::Sin(Phi);
    Double_t cosL = 1./TMath::Sqrt(1. + pars->tanl*pars->tanl);
    Double_t sinL = cosL*pars->tanl;
    // d(Phi,tanl)     d(Phi,tanl)      d(nG_x,nG_y,nG_y)  d(nL_x,nL_y,nL_y)
    // ---------- = ----------------  x ---------------- x ----------------- 
    // d(tY,tZ)     d(nG_x,nG_y,nG_z)   d(nL_x,nL_y,nL_y)    d(tY,tZ)
    //
    //            = dPhiTanldnG       x ROT              x dnLdtYtZ
    Double_t cosL2 = cosL*cosL;
    TRMatrix dPhiTanldnG(2,3,//nx     ny  nz
			 -sinPhi, cosPhi, 0.,        // Phi
			 0.     ,     0., 1./cosL2); // tanl
    dPhiTanldnG /= cosL;
    PrPC(SD2Sti,dPhiTanldnG);
    TRVector nG(3, cosPhi*cosL, sinPhi*cosL, sinL); PrPC(SD2Sti,nG);
    TRVector nL(3);
    _rot->MasterToLocalVect(nG.GetArray(),nL.GetArray());
    TRMatrix dnGdnL(3,3,_rot->GetRotationMatrix()); // L => G
    PrPC(SD2Sti,dnGdnL);
    Double_t nx = nL[0];
    Double_t tY = nL[1]/nx;
    Double_t tZ = nL[2]/nx;
    TRVector tN(3, 1., tY, tZ); PrPC(SD2Sti,tN);
    TRMatrix dnLdtYtZ(3,2,//  tY       tZ
		      -tY      ,      -tZ,  // nL_x
		      (1+tZ*tZ),   -tY*tZ,  // nL_y
		      -tY*tZ   ,(1+tY*tY)); // nL_z
    dnLdtYtZ *= TMath::Power(nx,3);                             PrPC(SD2Sti,dnLdtYtZ);
    TRMatrix dnGdtYtZ(dnGdnL,TRArray::kAxB,dnLdtYtZ);           PrPC(SD2Sti,dnGdtYtZ);
    TRMatrix dPhitanldtYtZ(dPhiTanldnG,TRArray::kAxB,dnGdtYtZ); PrPC(SD2Sti,dPhitanldtYtZ);
    Double_t dPSdPD[25] = {
      //1/p,                           tY,                          tZ, y, z
      0,                                0,                           0, 1, 0, // y
      0,                                0,                           0, 0, 1, // z
      0,               dPhitanldtYtZ(0,0),          dPhitanldtYtZ(0,1), 0, 0, // Phi
      -q/cosL,-q/p*sinL*dPhitanldtYtZ(1,0),-q/p*sinL*dPhitanldtYtZ(1,1), 0, 0, //-q/pT
      0,               dPhitanldtYtZ(1,0),          dPhitanldtYtZ(1,1), 0, 0};// tanl
    F() = TRMatrix(5,5, dPSdPD); // d(y,z,Phi,1/pT,tanl)/d(1/p,tY,tZ,y,z)
    PrPC(SD2Sti,F());
    PrPC(SD2Sti,source.C());
    TRSymMatrix RS(F(), TRArray::kAxSxAT,source.C()); PrPC(SD2Sti,RS);
    TRSymMatrix RS6(6,
		    0.,
		    0., RS(0,0),
		    0., RS(1,0), RS(1,1),
		    0., RS(2,0), RS(2,1), RS(2,2),
		    0., RS(3,0), RS(3,1), RS(3,2), RS(3,3),
		    0., RS(4,0), RS(4,1), RS(4,2), RS(4,3), RS(4,4));
    C() = RS6; PrPC(SD2Sti,C());
  }
  return *this;
}
//________________________________________________________________________________
FitParameters  &FitParameters::Dca2Sti(const FitParameters &source) {
  if (this != &source) {
    assert(0);
  }
  return *this;
}
//______________________  Dca ___________________________________________________
//________________________________________________________________________________
TRVector FitParameters::PxyzGDca() const {
  //       pars =>  Imp, Z, Psi, pTInv, tanL) pTInv = q/pT 
  ParamDca_t *pars = (ParamDca_t *) P().GetArray();
  Double_t pT = (TMath::Abs(pars->pTInv)<1e-6) ? 1e6: 1./TMath::Abs(pars->pTInv);
  Double_t pxyz[3] = {pT*TMath::Cos(pars->Psi), pT*TMath::Sin(pars->Psi), pT*pars->tanL};
  return TRVector(3,pxyz);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::PxyzGCovMatrixDca() const {
  //       pars =>  Imp, Z, Psi, pTInv, tanL) pTInv = q/pT 
  assert(0);
  return TRSymMatrix(3);
}
//________________________________________________________________________________
TRVector FitParameters::XyzGDca() const {
  assert(0);
  //       pars =>  Imp, Z, Psi, pTInv, tanL) pTInv = q/pT 
  ParamDca_t *pars = (ParamDca_t *) P().GetArray();
  Double_t xyz[3] = {-pars->Imp*TMath::Sin(pars->Psi), pars->Imp*TMath::Cos(pars->Psi), pars->Z};
  return TRVector(3,xyz);
}
//________________________________________________________________________________
TRSymMatrix FitParameters::XyzGCovMatrixDca() const {
  assert(0);
  //       pars =>  Imp, Z, Psi, pTInv, tanL) pTInv = q/pT 
  //  ParamDca_t *pars = (ParamDca_t *) P().GetArray();
  return TRSymMatrix(3);
}
//________________________________________________________________________________
FitParameters &FitParameters::SC2Dca(const FitParameters &source) {
  if (this != &source) {
    assert(0);
  }
  return *this;
}
//________________________________________________________________________________
FitParameters &FitParameters::SD2Dca(const FitParameters &source) {
  if (this != &source) {
    assert(0);
  }
  return *this;
}
//________________________________________________________________________________
const Char_t  *FitParameters::PrintpTasString() const {
  Double_t p         =     Charge()*pT();
  Double_t dpT = 100*dpTOverpT();
  assert(dpT >= 0);
  if (dpT > 9999.9) dpT = 9999.9;
  return Form(" pT %8.3f+-%6.1f",p,dpT);
}
//________________________________________________________________________________
#undef PrPC
