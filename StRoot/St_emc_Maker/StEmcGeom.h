/***************************************************************************
 *
 * $Id: StEmcGeom.h,v 1.3 1999/07/16 18:03:45 pavlinov Exp $
 *
 * Author:  Aleksei Pavlinov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcGeom.h,v $
 * Revision 1.3  1999/07/16 18:03:45  pavlinov
 * Little correction for StEclMake
 *
 * Revision 1.2  1999/07/02 03:01:56  pavlinov
 * Little corrections for Linux
 *
 * Revision 1.1  1999/07/01 16:17:57  pavlinov
 * class StEmcGeom was created and maker was remade for new maker scheme
 *
 **************************************************************************/
#ifndef STAR_StEmcGeom
#define STAR_StEmcGeom
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcGeom main class for <FONT COLOR="RED"> Geometry of BEMC for OFFline </FONT>      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <math.h>
#include "TArrayF.h"
class StEmcGeom {
private:
  Int_t   mDetector;// Detector's number => see emc/inc/emc_def.h
  Int_t   mNModule; // Number of modeles (120 is default)
  Int_t   mNEta;    // Number of eta bins
  Int_t   mNSub;    // Subdivision in eta bin
  Int_t   mNes;     // mNes  = mNEta * mNSubStEmcGeom.d
  Int_t   mNRaw;    // mNraw = mNes * mNModule
  Float_t mRadius;  // Distance to beam axis
  Float_t mYWidth;  // Half Width of module for mRadius
  TArrayF mPhiOffset;  // Phi offset form numbering scheme 
  TArrayF mPhiStep;    // Step for transition to Star Cordinate System(+/-2pi/60.)
  
  TArrayF mZlocal;  // Array of z   coordinates (system of single module) 
  TArrayF mYlocal;  // Array of y   coordinates (system of single module) 
  TArrayF mEta;     // Array of eta coordinates (system of single module)
  TArrayF mPhi;     // Array of phi coordinates (system of single module)

public: 
  StEmcGeom(const Int_t );
  StEmcGeom(const Char_t*);
  virtual ~StEmcGeom();

  Int_t    Detector() const;
  Int_t    NModule()  const;
  Int_t    NEta()  const;
  Int_t    NSub()  const;
  Int_t    Nes()  const;
  Int_t    NRaw()  const;
  Float_t  Radius()  const;
  Float_t  YWidth() const;
  void     setDetector(const Int_t);

  Int_t    checkModule(const Int_t );
  Int_t    checkEta(const Int_t );
  Int_t    checkSub(const Int_t );
  Int_t    checkId(const Int_t );

  Int_t    getBin(const Int_t,Int_t &,Int_t &,Int_t &);        // From raw# to bin#; 
  Int_t    getId(const Int_t,const Int_t,const Int_t,Int_t &); // From bin# to raw#;
  
  Int_t    getZlYl(const Int_t,Float_t &,Float_t &);  // Get (x,y) local in the raw#;
  Int_t    getEta(const Int_t, const Int_t, Float_t &);
  Int_t    getPhi(const Int_t, const Int_t, Float_t &);
  Int_t    getEtaPhi(const Int_t, Float_t &,  Float_t &);

  void     initGeom(const Int_t);
  void     initBEMCorBPRS();
  void     initBSMDE();
  void     initBSMDP();
  void     printGeom();
  ClassDef(StEmcGeom,1)                      // Standard Root macro;
};

inline Int_t   StEmcGeom::Detector() const {return mDetector;}
inline Int_t   StEmcGeom::NModule()  const {return mNModule;}
inline Int_t   StEmcGeom::NEta()     const {return mNEta;}
inline Int_t   StEmcGeom::NSub()     const {return mNSub;}
inline Int_t   StEmcGeom::Nes()      const {return mNes;}
inline Int_t   StEmcGeom::NRaw()     const {return mNRaw;}
inline Float_t StEmcGeom::Radius()   const {return mRadius;}
inline Float_t StEmcGeom::YWidth()   const {return mYWidth;}
inline void    StEmcGeom::setDetector(const Int_t val) { mDetector = val;} 

// _____________________________________________________________________
inline Int_t   StEmcGeom::checkModule(const Int_t m)
{
  if(m>=1 && m<=mNModule) return 0;
  else cout<<" Bad module# "<<m<<endl; return 1;
}
// _____________________________________________________________________
inline Int_t   StEmcGeom::checkEta(const Int_t e)
{
  if(e>=1 && e<=mNEta) return 0;
  else cout<<" Bad eta# "<<e<<endl; return 1;
}
// _____________________________________________________________________
inline Int_t   StEmcGeom::checkSub(const Int_t s)
{
  if(s>=1 && s<=mNSub) return 0;
  else cout<<" Bad sub# "<<s<<endl; return 1;
}
// _____________________________________________________________________
inline Int_t   StEmcGeom::checkId(const Int_t rid)
{
  if(rid>=0 && rid<mNRaw) return 0;
  else cout<<" Bad raw# "<<rid<<endl; return 1;
}
// _____________________________________________________________________
inline Int_t   StEmcGeom::getBin(const Int_t rid,Int_t &m, Int_t &e, Int_t &s)
{
  if(!checkId(rid)) { 
    Int_t j = rid%mNes;     // =mod(rid,mNes)
    m = rid/mNes + 1;
    e = j/mNSub  + 1; 
    s = j%mNSub  + 1;
    //    if(checkModule(m) || checkEta(e) || checkSub(s)) {cout<<"Bad getBin"<<endl; return 1;} // Only for testing
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getId(const Int_t m,const Int_t e,const Int_t s, Int_t &rid)
{ // Check boundary
  if(!checkModule(m) && !checkEta(e) && !checkSub(s)){
    rid = (m-1)*mNes + (e-1)*mNSub + s-1;
    //checkId(rid);  // Only for testing
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getZlYl(const Int_t rid, Float_t &zl, Float_t &yl)
{
  Int_t m, e, s;

  if(!getBin(rid,  m,  e, s)){
    zl = mZlocal[e-1]; 
    yl = mZlocal[s-1];
    return 0;
  }
  else return 1;
} 
// _____________________________________________________________________
inline Int_t StEmcGeom::getEta(const Int_t m, const Int_t  e, Float_t &eta)
{
  if(!checkModule(m) && !checkEta(e)){
    if(m <= mNModule/2) eta =  mEta[e-1];  // West part of EMC
    else                eta = -mEta[e-1];  // East part of EMC
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getPhi(const Int_t m, const Int_t  s, Float_t &phi)
{
  Int_t iphi, im;
  if(!checkModule(m) && !checkSub(s)){
    Float_t phiW;       // phi in system of module

    if(m <= mNModule/2) {phiW = mPhi[s-1]; im = 0; iphi=m-1;} // West part of EMC
    else {phiW = -mPhi[s-1]; im = 1; iphi=m-mNModule/2-1;}     // East part of EMC    
    
    phiW += mPhiOffset[im] + mPhiStep[im]*iphi;
    //    Double_t pi=TMath::Pi(); 
    while(phiW >=  M_PI) phiW -= 2.*M_PI;  // Range for phi
    while(phiW <  -M_PI) phiW += 2.*M_PI;  // from -pi to pi;
    phi = phiW;
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getEtaPhi(const Int_t rid, Float_t &eta, Float_t &phi)
{
  Int_t m=0, e=0, s=0;
  if(!getBin(rid, m, e, s)) {
    getEta(m, e, eta);
    getPhi(m, s, phi);
    return 0;
  }
  else return 1;
}

#endif
