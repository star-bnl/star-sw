#ifndef StGeanePropagator_h
#define StGeanePropagator_h
// $Id: StGeanePropagator.h,v 1.8 2012/06/11 23:21:12 fisyak Exp $
#include "TNamed.h"
#include "TVector3.h"
#include "TGeant3.h"
#include "TRSymMatrix.h"
#include "TRMatrix.h"
#include "TRVector.h"
#include <vector>
#include "TGeoMatrix.h"
#include "TString.h"
#include "TGeant3TGeo.h"
#include "TArrayI.h"
struct SensVol_t {
  SensVol_t(const Char_t *n, Int_t i, Int_t v) : name(n), ivol(i), numv(v) {}
  TString name;
  Int_t   ivol;
  Int_t   numv;
  Bool_t operator==(const SensVol_t &vol) const;
};
class StGeanePropagator : public TNamed {
 public:
  static StGeanePropagator* instance();
  void SetSensVolume(const Char_t *name = "tpad", Int_t ivol = 1, Int_t numv = 0);
  void SetVolumes();
  void SetMinTrackingRadius(Double_t r =   0); 
  void SetMaxTrackingRadius(Double_t r = 210);
  void SetMaxTrackingZ     (Double_t z = 210); 
  void SetXYZ(const TVector3 &xyz)                 {fxyzG = xyz;}
  void SetXYZ(Double_t *xyz)                       {fxyzG = TVector3(xyz);}
  void SetXYZ(Float_t *xyz)                        {fxyzG = TVector3(xyz);}
  void SetXYZ(Double_t x, Double_t y, Double_t z)  {fxyzG.SetXYZ(x,y,z);}
  void SetpXYZ(const TVector3 &p)                  {fpxyzG = p;}
  void SetpXYZ(Double_t *pxyz)                     {fpxyzG = TVector3(pxyz);}
  void SetpXYZ(Float_t *pxyz)                      {fpxyzG = TVector3(pxyz);}
  void SetpXYZ(Double_t x, Double_t y, Double_t z) {fpxyzG.SetXYZ(x,y,z);}
  void SetParticle(Int_t p = 5)                    {fKine = p;}
  void SetRC(TRSymMatrix &RC){fRC = RC;}
  void SetRC(Double_t   *rc) {fRC.Set(5,rc);}
  void SetRC(Float_t    *rc) {fRC = TRSymMatrix(5,rc);}
  void SetPD(TRVector    &PD, TGeoHMatrix *rot = 0);
  void SetRD(TRSymMatrix &RD){fRD = RD;}
  void SetRD(Double_t   *rd) {fRD.Set(5,rd);}
  void SetRD(Float_t    *rd) {fRD = TRSymMatrix(5,rd);}
  void SetTRP(TRMatrix   &A) {fTRP = A;}
  void SetDebug(Int_t p)     {fDebug = p;}
  TVector3 &GetXYZ()           {return fxyzG;}
  void GetXYZ(Double_t  *xyz)  {return fxyzG.GetXYZ(xyz);}
  void GetXYZ(Float_t   *xyz)  {return fxyzG.GetXYZ(xyz);}
  TVector3 &GetpXYZ()          {return fpxyzG;}
  void GetpXYZ(Double_t *pxyz) {return fpxyzG.GetXYZ(pxyz);}
  void GetpXYZ(Float_t  *pxyz) {return fpxyzG.GetXYZ(pxyz);}
  const Char_t *GetPath()      {return fPath.Data();}
  TRSymMatrix &GetRC()         {return fRC;}
  TRSymMatrix &GetRD()         {return fRD;}
  TRMatrix    &GetTRP()        {return fTRP;}
  TRMatrix    &GetdDdD()       {return fTRP;}
  TRVector    &GetPD()         {return fPD;}
  Float_t      Charge()        {return fgckine ? fgckine->charge : 0;}
  Int_t Propagate(const Char_t *opt="");
  Int_t Propagate(const TGeoHMatrix &rotI,const TGeoHMatrix &rotO,const Char_t *opt);
  Int_t Propagate(const TGeoHMatrix &rotI,Double_t length,const Char_t *opt);
  Int_t Debug()                {return fDebug;}
 protected:
  StGeanePropagator(const Char_t *name="GeaneP", const Char_t *title = "") : 
    TNamed(name,title), fRC(5), fRD(5), fTRP(5,5), fNoSensVol(0), fNumv(0) {}
 public:
  virtual ~StGeanePropagator() {SafeDelete(fgInstance); if (fCnamv) {delete [] fCnamv; fCnamv = 0;}}
 private:
  static TVector3     fxyzG; 
  static TVector3     fpxyzG;
  static Int_t        fKine; // GEANT particle ID
  std::vector<SensVol_t>   fSensVolVec;
  TRSymMatrix         fRC; // error matrix in SC
  TRVector            fPD; // parameters on Plane
  TRSymMatrix         fRD; // error matrix on plane
  TRMatrix            fTRP;// transport matrix
  static TGeoHMatrix  *fRot;// current rotation matrix
  TString             fPath;// current path
  UInt_t              fNoSensVol;
  Char_t            **fCnamv;          
  TArrayI             fNumv;
  TArrayI             fIvol;
  static StGeanePropagator *fgInstance;
  static TGeant3TGeo       *fgGeant3;
  static Gcflag_t          *fgcflag;
  static Gckine_t          *fgckine;
  static Gctmed_t          *fgctmed;
  static Erwork_t          *fgerwork;
  static Ertrio_t 	   *fgertrio;
  static Trcom3_t          *fgtrcom3;
  static Eropts_t          *fgeropts;
  static Eroptc_t          *fgeroptc;
  static Int_t              fDebug;
  ClassDef(StGeanePropagator,0)
};
// $Log: StGeanePropagator.h,v $
// Revision 1.8  2012/06/11 23:21:12  fisyak
// std namespace
//
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
//
// Revision 1.1  2009/04/21 20:51:07  fisyak
// GeanE propagator, starting version
//
#endif
