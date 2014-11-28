// $Id: StTpcAlignerMaker.h,v 1.7 2014/09/10 13:54:58 fisyak Exp $
#ifndef STAR_StTpcAlignerMaker
#define STAR_StTpcAlignerMaker
/*!
 *                                                                     
 * \class  StTpcAlignerMaker
 * \author fisyak
 * \date   2004/10/01
 * \brief  virtual base class for Maker
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StTpcAlignerMaker virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StTpcAlignerMaker
 *
 *
 */                                                                      
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "TRSymMatrix.h"
#include "TRMatrix.h"
#include "TList.h"
#include "TString.h"
#include "TNamed.h"
#include "THelixTrack.h"
#include "TGeoMatrix.h"
class StTpcHit;
//________________________________________________________________________________
class HelixPar_t : public TObject {
 public:
  HelixPar_t() {Clear();}
  virtual ~HelixPar_t() {}
  Char_t         beg[1]; // !
  Int_t          sector;
  Double_t       Rho; // curvature
  Double_t       x, y, z;
  Double_t       nx, ny, nz;
  Double_t      dRho;
  //  StThreeVectorD pxyz; // Direction
  Double_t     *pxyz() {return &nx;}
  Double_t      *xyz() {return &x;}
  //  StThreeVectorD xyz;  // Coordinates
  Double_t       fCov[15];  // Covarianve matrix from Helix fit for (X,Z,dirX,dirY,dirZ);
  Double_t       Chi2;
  Int_t          Ndf;
  Int_t          Npoints;
  Int_t          Nused;
  Char_t         end[1]; // !
  void           Clear(Option_t *opt = 0) {if (opt); memset(beg, 0, end-beg);}
  HelixPar_t    &operator=(const THelixFitter &v);
  ClassDef(HelixPar_t,1)
};
ostream&  operator<<(ostream& os, const HelixPar_t v);
//________________________________________________________________________________
class Hit_t : public TObject {
 public:
  Int_t    row;
  Double_t x, y, z;
  Double_t err2xy, err2z;
  StTpcHit *hit;
  ClassDef(Hit_t,1); 
};
//________________________________________________________________________________
class StTpcInOutMatch : public TObject {
public:
  StTpcInOutMatch() {}
  ~StTpcInOutMatch() {}
  Char_t         beg[1]; // !
  Int_t    TriggerId;
  Double_t field;
  Double_t charge;
  Int_t    NoFitPoints;
  Double_t pX, pY, pZ;
  //  StThreeVectorF pxyzG;
  Char_t         end[1]; // !
  HelixPar_t In;
  HelixPar_t Out;
  void        Clear(Option_t *opt = 0) {if (opt); memset(beg, 0, end-beg); In.Clear(); Out.Clear();}
  ClassDef(StTpcInOutMatch,1)
};
//________________________________________________________________________________
class SectorSegment : public TNamed {
 public: 
  SectorSegment(Int_t s, Int_t t = 0) : fRowMin(99), fRowMax(-1), fSector(s), fStatus(-1) {SetName(Form("Sector_%02i_%i",fSector,t));}
  virtual ~SectorSegment() {}
  virtual Bool_t   IsSortable() const { return kTRUE; }
  Int_t   Sector() const {return fSector;}
  TList *List() {return &fList;}
  THelixFitter &Helix()       {return *&fHelix;}
  void SetSector(Int_t s, Int_t t = 0) {fSector = s; SetName(Form("Sector_%02i_%i",fSector,t));}
  void SetStatus(Int_t k) {fStatus = k;}
  Int_t Status() {return fStatus;}
  virtual Int_t    Compare(const  TObject *obj) const {
    SectorSegment *hit = (SectorSegment *) obj;
    if (Sector() > hit->Sector()) return kTRUE;
    return kFALSE;
  }
  virtual void  Print(Option_t *option="") const;
  Int_t  fRowMin;
  Int_t  fRowMax;
  Double_t fXmin;
  Double_t fXmax;
  HelixPar_t     HelixSmin; // at fRowMin
  HelixPar_t     HelixSmax; // at fRowMax
 private:
  Int_t fSector;
  TList fList;
  THelixFitter fHelix;
  Int_t fStatus;
  ClassDef(SectorSegment,1)
};
//________________________________________________________________________________
class StTpcW2SMatch : public TObject {
 public:
  StTpcW2SMatch() {}
  ~StTpcW2SMatch() {}
  Int_t          TriggerId;
  TGeoHMatrix    RW2S;      // W->S
  HelixPar_t     HelixW; // parameters to used to predict
  HelixPar_t     HelixU; // parameters to used to predict
  HelixPar_t     HelixS; // -"- measurement
  void        Clear(Option_t *opt = 0) {if (opt);  HelixW.Clear(); HelixS.Clear();}
  ClassDef(StTpcW2SMatch,1)
};
//________________________________________________________________________________
class StTpcAlignerMaker : public StMaker {
 public:
  StTpcAlignerMaker(const char *name="TpcAligner") : StMaker(name), fTpcInOutMatch(0) , fTpcW2SMatch(0){}
  virtual       ~StTpcAlignerMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  static  TRMatrix &GetSti2R(Double_t nx, Double_t ny, Double_t nz);
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTpcAlignerMaker.h,v 1.7 2014/09/10 13:54:58 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  static Double_t PhiFromBTofTray(Int_t tray);
  static Int_t    TpcSectorFromBTofTray(Int_t tray);
 private:
  StTpcInOutMatch *fTpcInOutMatch;
  StTpcW2SMatch *fTpcW2SMatch;
  ClassDef(StTpcAlignerMaker,0)
};

#endif
// $Log: StTpcAlignerMaker.h,v $
// Revision 1.7  2014/09/10 13:54:58  fisyak
// Freeze
//
// Revision 1.6  2011/12/16 20:35:23  fisyak
// Freeze
//
// Revision 1.5  2011/08/23 20:29:06  fisyak
// Add rejection for bad segments
//
// Revision 1.4  2011/08/22 13:37:25  fisyak
// Freeze Super Sector version
//
// Revision 1.3  2011/06/08 21:52:47  fisyak
// Freeze version with ToF
//
// Revision 1.2  2011/04/22 16:03:53  fisyak
// Replace NTuple by TTree, use errors from THelixFit
//
// Revision 1.1.1.1  2004/10/28 00:26:46  fisyak
