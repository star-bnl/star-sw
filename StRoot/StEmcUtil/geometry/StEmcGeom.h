/***************************************************************************
 *
 * $Id: StEmcGeom.h,v 1.5 2007/07/16 21:24:59 kocolosk Exp $
 *
 * Author:  Aleksei Pavlinov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcGeom.h,v $
 * Revision 1.5  2007/07/16 21:24:59  kocolosk
 * added projection against sub == -1 case in getId(phi,eta,softId).  Thanks Pibero
 *
 * Revision 1.4  2007/04/04 17:32:11  kocolosk
 * Added softId-based versions of getEta, getTheta, and getPhi.  Also added getId(phi,eta,&softId).  Implemented const-correctness and used meaningful argument names in method declarations to improve readability
 *
 * Revision 1.3  2004/08/19 17:31:45  pavlinov
 * getBin(const Float_t phi, const Float_t eta, Int_t &m,Int_t &e,Int_t &s) works bsmde too - request of Dmitry Arkhipkin
 *
 * Revision 1.2  2003/09/02 17:58:01  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2003/01/23 01:30:28  suaide
 * moving to sub directories
 *
 * Revision 1.13  2001/11/21 18:56:19  suaide
 * added methods to get eta and phi bins boundaries
 *
 * Revision 1.12  2001/09/24 17:34:51  akio
 * Dirty bug fix.
 *
 * Revision 1.11  2001/09/22 00:28:58  pavlinov
 * No public constructor for StEmcGeom
 *
 * Revision 1.10  2001/08/08 00:33:15  pavlinov
 * New Jose's ID
 *
 * Revision 1.9  2001/07/30 00:16:22  pavlinov
 * Correct numbering scheme for BSMDE
 *
 * Revision 1.8  2001/07/13 18:35:20  pavlinov
 * New version of methods getBin and getId for BSMDE and BSMDP
 *
 * Revision 1.7  2001/04/26 14:23:40  akio
 * Quick and dirty fix for crashing non-bfc chain
 *
 * Revision 1.6  2001/04/25 01:03:10  pavlinov
 * Clean up
 *
 * Revision 1.5  2001/03/22 21:50:35  pavlinov
 * Clean up for mdc4
 *
 * Revision 1.4  2001/03/15 23:47:20  pavlinov
 * Fixed error on SUN compiler
 *
 * Revision 1.3  2001/03/15 20:56:11  pavlinov
 * Jose's scheme is default
 *
 * Revision 1.2  2001/03/15 00:57:28  pavlinov
 * Created new methods getBinEnv and getIdEnv
 *
 * Revision 1.1  2000/06/20 17:11:25  pavlinov
 * Move StEmcGeom.h from St_emc_Maker to StEmcUtil
 *
 * Revision 1.11  2000/05/23 14:35:01  pavlinov
 * Clean up for SUN
 *
 * Revision 1.10  2000/05/18 17:07:30  pavlinov
 * Fixed error for methods getXYZ(...)
 *
 * Revision 1.9  2000/04/27 01:39:15  pavlinov
 * Cleanup for SUN
 *
 * Revision 1.8  2000/04/25 17:02:06  pavlinov
 * Added methods for gettinng x,y,z from volume ID
 *
 * Revision 1.7  2000/04/21 17:43:01  pavlinov
 * Added methods for for decoding Geant volume Id
 *
 * Revision 1.6  2000/04/18 20:38:09  pavlinov
 * Added ctor from Geant geometry
 *
 * Revision 1.5  2000/04/11 19:48:40  pavlinov
 * Merge versions of Pavlinov and Ogawa
 *
 * Revision 1.4  2000/01/29 00:04:57  akio
 * temprary fix for endcap. need more work, but no more junk messages and crash
 *
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
#include <Stiostream.h>
#include "math_constants.h"
#include <math.h>
#include <TArrayF.h>
#include <TString.h>
#include "tables/St_calb_calg_Table.h"
#include "tables/St_calb_calr_Table.h"
#include "StMessMgr.h"

class StMaker;
class TDataSet;

class StEmcGeom {
private:
  static StEmcGeom *mGeom[8]; //! 

  StMaker*      mChain;    //!
  TDataSet*     mGeantGeom;//!
  St_calb_calg* mCalg;     //!
  calb_calg_st* mCalg_st;  //!
  St_calb_calr* mCalr;     //!  
  calb_calr_st* mCalr_st;  //!

  void    defineDefaultCommonConstants();
  void    defineCommonConstants();
  void    defineModuleGridOnPhi();
  Float_t relativeError(Float_t, Float_t) const;
  //  static Int_t   getIndex(const Float_t x, TArrayF &arr);
  Int_t   getIndex(const Float_t &x, const TArrayF &arr) const;


protected:
  TString mMode;     // Empty, "geant" or "db"
  Int_t   mDetector; // Detectors number => see emc/inc/emc_def.h
  Int_t   mNModule;  // Number of modeles (120 is default)
  Int_t   mNEta;     // Number of eta bins
  Int_t   mNSub;     // Subdivision in phi bin
  Int_t   mNes;      // mNes  = mNEta * mNSub
  Int_t   mNRaw;     // mNraw = mNes * mNModule
  Float_t mRadius;   // Distance to beam axis
  Float_t mYWidth;   // Half Width of module for mRadius
  Float_t mEtaMax;
  Float_t mEtaMin;   // 
  TArrayF mPhiOffset;  // Phi offset from numbering scheme for center of tower
  TArrayF mPhiStep;    // Step for transition to Star Cordinate System(-/+2pi/60)
  TArrayF mPhiBound;   // Phi offset from numbering scheme for edge of tower
  Float_t mPhiStepHalf; // 2pi/(60*2)
  
  TArrayF mZlocal;    // Array of z   coordinates (system of single module) 
  TArrayF mYlocal;    // Array of y   coordinates (system of single module) 
  TArrayF mEta;       // Array of eta coordinates of center modules
  TArrayF mEtaB;      // Array of eta boundaries
  TArrayF mPhi;       // Array of phi coordinates (system of single module)
  TArrayF mPhiModule; // Angle of center of module in STAR system
  TArrayF mYB;      //  25-jul-2001 
  TArrayF mPhiB;    // 

  Int_t   mMaxAdc;    // Range of ADC for each detector

public: 
	StEmcGeom(const Int_t );
	StEmcGeom(const Char_t*);
	StEmcGeom(const Int_t ,const Char_t*);

	static StEmcGeom *instance(const Int_t det);
	static StEmcGeom *getEmcGeom(const Int_t det);
	static StEmcGeom *instance(const Char_t* cdet);
	static StEmcGeom *getEmcGeom(const Char_t* cdet);
	static StEmcGeom *instance(const Int_t det, const Char_t* mode);
	static StEmcGeom *getEmcGeom(const Int_t det, const Char_t* mode);

	static Int_t getDetNumFromName(const Char_t *cdet);

	virtual ~StEmcGeom();

	const TString* Mode() const;
	Int_t    Detector() const;
	Int_t    NModule()  const;
	Int_t    NEta()  const;
	Int_t    NSub()  const;
	Int_t    Nes()  const;
	Int_t    NRaw()  const;
	Float_t  Radius()  const;
	Float_t  YWidth() const;
	Float_t  EtaMax() const;
	Float_t  EtaMin() const;
	const Float_t* PhiModule() const;
	const Float_t* PhiOffset() const;
	const Float_t* PhiStep() const;
	const Float_t* PhiBound() const;
	const Float_t* Zlocal() const; 
	const Float_t* Eta() const;
	const Float_t* Ylocal() const;  
	const Float_t* Phi() const;

	const Float_t* EtaB() const; // Eta boundaries AASUAIDE
	const Float_t* PhiB() const; // Phi boundaries AASUAIDE

	void     setDetector(const Int_t val);
	void     setRadius(const Float_t val);
	void     setYWidth(const Float_t val);

	//check that the supplied value is in an acceptable range
	Int_t    checkModule(const Int_t m) const;
	Int_t    checkEta(const Int_t e) const;
	Int_t    checkSub(const Int_t s) const;
 	Int_t    checkId(const Int_t softId) const;

	//convert (eta,phi) to (m,e,s) or softId
	Int_t    getBin(const Float_t phi, const Float_t eta, Int_t &m, Int_t &e, Int_t &s) const;
	Int_t    getId(const Float_t phi, const Float_t eta, Int_t &softId) const;

	//convert (m,e,s) <-> softId
	Int_t    getBin(const Int_t softId, Int_t &m, Int_t &e, Int_t &s) const;
	Int_t    getId(const Int_t m, const Int_t e, const Int_t s, Int_t &softId) const;

	Int_t    getVolIdBemc(const Int_t ivid, Int_t &module, Int_t &eta, Int_t &sub, Int_t &detector);
	Int_t    getVolIdBsmd(const Int_t ivid, Int_t &module, Int_t &eta, Int_t &sub, Int_t &detector);
	Int_t    getVolId(const Int_t ivid, Int_t &module, Int_t &eta, Int_t &sub, Int_t &det);

	Int_t    getZlYl(const Int_t softId, Float_t &zl, Float_t &yl) const;
	void     getXYZ(const Int_t m, const Int_t e, const Int_t s, Float_t &x, Float_t &y, Float_t &z) const;
	Int_t    getXYZ(const Int_t softId, Float_t &x, Float_t &y, Float_t &z) const;
	Int_t    getXYZfromGeant(const Int_t ivid, Float_t &x, Float_t &y, Float_t &z);

	Int_t    getEta(const Int_t m, const Int_t e, Float_t &eta) const;
	Int_t    getEta(const Int_t softId, Float_t &eta) const;
	
	Int_t    getTheta(const Int_t m, const Int_t e, Float_t &theta) const;
	Int_t    getTheta(const Int_t softId, Float_t &theta) const;
	
	Int_t    getPhi(const Int_t m, const Int_t s, Float_t &phi) const;
	Int_t    getPhi(const Int_t softId, Float_t &phi) const;
		
	Int_t    getEtaPhi(const Int_t softId, Float_t &eta, Float_t &phi) const;
	
	Int_t    getPhiModule(const Int_t m, Float_t &phi) const;

	Int_t    getMaxAdc() const {return mMaxAdc;}

	void     initGeom(const Int_t);
	void     initBEMCorBPRS();
	void     initBSMDE();
	void     initBSMDP();
	void     initEEMCorEPRS();
	void     initESMDE();
	void     initESMDP();
	void     printGeom() const;
	void     print() const {printGeom();}
	void     compare(const StEmcGeom &, Bool_t) const;
	void     compare(const StEmcGeom * const g, Bool_t key) const {compare(*g,key);};
	void     printError(Float_t) const;

	Float_t  toDeg(const Float_t angR) const {return C_DEG_PER_RAD*angR;} // Service
	Float_t  toRad(const Float_t angD) const {return angD/C_DEG_PER_RAD;} // functions
	void     getGeantGeometryTable();

 	ClassDef(StEmcGeom,1)                      // Standard Root macro;
};

inline const TString* StEmcGeom::Mode() const {return &mMode;}
inline Int_t   StEmcGeom::Detector() const {return mDetector;}
inline Int_t   StEmcGeom::NModule()  const {return mNModule;}
inline Int_t   StEmcGeom::NEta()     const {return mNEta;}
inline Int_t   StEmcGeom::NSub()     const {return mNSub;}
inline Int_t   StEmcGeom::Nes()      const {return mNes;}
inline Int_t   StEmcGeom::NRaw()     const {return mNRaw;}
inline Float_t StEmcGeom::Radius()   const {return mRadius;}
inline Float_t StEmcGeom::YWidth()   const {return mYWidth;}
inline Float_t StEmcGeom::EtaMax()   const {return mEtaMax;}
inline Float_t StEmcGeom::EtaMin()   const {return mEtaMin;}
inline const Float_t* StEmcGeom::PhiModule() const {return mPhiModule.GetArray();} 
inline const Float_t* StEmcGeom::PhiOffset() const {return mPhiOffset.GetArray();}
inline const Float_t* StEmcGeom::PhiStep() const {return mPhiStep.GetArray();}
inline const Float_t* StEmcGeom::PhiBound() const {return mPhiBound.GetArray();}
inline const Float_t* StEmcGeom::Zlocal() const {return mZlocal.GetArray();} 
inline const Float_t* StEmcGeom::Eta() const {return mEta.GetArray();} 
inline const Float_t* StEmcGeom::Ylocal() const {return mYlocal.GetArray();}
inline const Float_t* StEmcGeom::Phi() const {return mPhi.GetArray();} 

inline const Float_t* StEmcGeom::EtaB() const {return mEtaB.GetArray();}
inline const Float_t* StEmcGeom::PhiB() const {return mPhiB.GetArray();}

inline void    StEmcGeom::setDetector(const Int_t val) { mDetector = val;} 
inline void    StEmcGeom::setRadius(const Float_t val) { mRadius = val;}
inline void    StEmcGeom::setYWidth(const Float_t val) { mYWidth = val;}

// _____________________________________________________________________
inline Int_t StEmcGeom::checkModule(const Int_t m) const
{
  if(m>=1 && m<=mNModule) return 0;
  else {LOG_ERROR<<" Bad module# "<<m<<"/"<<mNModule<<" in Detector "<<mDetector<<endm; return 1;}
}
// _____________________________________________________________________
inline Int_t StEmcGeom::checkEta(const Int_t e) const
{
  if(e>=1 && e<=mNEta) return 0;
  else {LOG_ERROR<<" Bad eta# "<<e<<endm; return 1;}
}
// _____________________________________________________________________
inline Int_t StEmcGeom::checkSub(const Int_t s) const
{
  if(s>=1 && s<=mNSub) return 0;
  else {LOG_ERROR<<" Bad sub# "<<s<<endm; return 1;}
}
// _____________________________________________________________________
inline Int_t StEmcGeom::checkId(const Int_t softId) const
{
	if(softId>=1 && softId<=mNRaw) return 0;
	else {LOG_ERROR<<" Bad raw# "<<softId<<endm; return 1;}
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getBin(const Float_t phi, const Float_t eta, Int_t &m, Int_t &e, Int_t &s) const
{
//
/// Transition from phi and eta to bin # => 3-Aug-1999 for StTbmMaker
//  19-aug-2004 - for all detectors; request of Dmitry Arkhipkin
//  mEtaMin=0 for BEMC, BPRS, BSMDP; mEtaMin=  for BMSDE
//

  Float_t phiw, sw;
  if(mEtaMin<eta && eta<=mEtaMax) {      // First Barrel
    e    = getIndex(eta, mEtaB); 
    phiw = mPhiBound[0] - phi;
    if(phiw<0.0) phiw = phiw + C_2PI; // 0<phiw<=2.*pi =>must be
    if(phiw<0.0 || phiw>C_2PI) printf(" phi %f eta %f \n",phi,eta); // For testing 
    m  = int(-phiw/mPhiStep[0]) + 1;

    sw   = fmod(phiw, fabs(mPhiStep[0]));
    sw   = sw - mPhiStepHalf;
    s    = getIndex(sw, mPhiB);

    return 0;

  } else if(-mEtaMax<=eta && eta<-mEtaMin) { // Second Barrel
    e    = getIndex(fabs(eta), mEtaB); 
    phiw = mPhiBound[1] - phi;
    if(phiw<0.0) phiw = phiw + C_2PI; // 0<phiw<=2.pi =>must be
    if(phiw<0.0 || phiw>C_2PI) printf(" phi %f eta %f \n",phi,eta); // For testing 
    m  = 120 - int(phiw/fabs(mPhiStep[1]));

    sw  = fmod(phiw, fabs(mPhiStep[0]));
    sw  = -(sw - mPhiStepHalf);
    s   = getIndex(sw, mPhiB);

    return 0;

  } else return 1;                      // Out of Bemc
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getId(const Float_t phi, const Float_t eta, Int_t &softId) const
{
	Int_t m,e,s;
	if(getBin(phi,eta,m,e,s) == 0 && s != -1) {
		return getId(m,e,s,softId);
	}
	return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getZlYl(const Int_t softId, Float_t &zl, Float_t &yl) const
{
  Int_t m, e, s;
  if(!getBin(softId,  m,  e, s)){
    zl = mZlocal[e-1]; 
    yl = mYlocal[s-1];
    return 0;
  }
  else return 1;
} 
// _____________________________________________________________________
inline void StEmcGeom::getXYZ(const Int_t m, const Int_t e, const Int_t s, Float_t &x,Float_t &y,Float_t &z) const
{
  Float_t phi;
  if(m<=60) z = mZlocal[e-1];
  else      z =-mZlocal[e-1];
  getPhi(m,s,phi);
  x = mRadius*cos(phi); 
  y = mRadius*sin(phi);
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getXYZ(const Int_t softId, Float_t &x,Float_t &y,Float_t &z) const
{
  Int_t m, e, s;
  if(!getBin(softId,  m,  e, s)){
    getXYZ(m,e,s, x,y,z);
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getXYZfromGeant(const Int_t ivid,Float_t &x,Float_t &y,Float_t &z)
{
  Int_t m, e, s, det;
  if(getVolId(ivid, m,e,s,det) == 0){
    getXYZ(m,e,s, x,y,z);
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getEta(const Int_t m, const Int_t  e, Float_t &eta) const
{
  if(!checkModule(m) && !checkEta(e)){
    if(m <= mNModule/2) eta =  mEta[e-1];  // West part of EMC
    else                eta = -mEta[e-1];  // East part of EMC
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getEta(const Int_t softId, Float_t &eta) const
{
	Int_t m,e,s;
	if(getBin(softId,m,e,s) == 0) {
		return getEta(m,e,eta);
	}
	return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getTheta(const Int_t m, const Int_t  e, Float_t &theta) const
{
  Float_t etaw;
  if(!getEta(m,e, etaw)) { theta = 2.*atan(exp(-etaw)); return 0;}  
  else return 1; 
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getTheta(const Int_t softId, Float_t &theta) const
{
	Int_t m,e,s;
	if(getBin(softId,m,e,s) == 0) {
		return getTheta(m,e,theta);
	}
	return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getPhi(const Int_t m, const Int_t  s, Float_t &phi) const
{
  //
  // -pi <= phi < pi
  //
  Int_t iphi, im;
  if(!checkModule(m) && !checkSub(s)){
    Double_t phiW;       // phi in system of module

    // change sign befor -mPhi
    if(m <= mNModule/2) {phiW = -mPhi[s-1]; im = 0; iphi=m-1;}  // West part of EMC
    else {phiW = mPhi[s-1]; im = 1; iphi=m-mNModule/2-1;}       // East part of EMC    
    
    phiW += mPhiOffset[im] + mPhiStep[im]*iphi;

    while(phiW >= C_PI) phiW -= C_2PI;
    while(phiW < -C_PI) phiW += C_2PI;
    if(phiW > (C_PI-0.0001)) phiW = -C_PI; // -pi<=phi<phi

    phi = phiW;
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getPhi(const Int_t softId, Float_t &phi) const
{
	Int_t m,e,s;
	if(getBin(softId,m,e,s) == 0) {
		return getPhi(m,s,phi);
	}
	return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getEtaPhi(const Int_t softId, Float_t &eta, Float_t &phi) const
{
  Int_t m=0, e=0, s=0;
  if(!getBin(softId, m, e, s)) {
    getEta(m, e, eta);
    getPhi(m, s, phi);
    return 0;
  }
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getPhiModule(const Int_t m, Float_t &phi) const
{
  if(!checkModule(m)) { phi=mPhiModule[m-1]; return 0;}
  else return 1;
}
// _____________________________________________________________________
inline Int_t StEmcGeom::getIndex(const Float_t &x, const TArrayF &arr) const
{
  // Arr is array of boundaries
  for(Int_t i=1; i<arr.GetSize(); i++){
    if(x>=arr[i-1] && x<arr[i]) return i; // x is in i cell
  }
  return -1;
}

#ifndef WSUJoseMarch
inline Int_t StEmcGeom::getId(const Int_t m, const Int_t e, const Int_t s, Int_t &softId) const
{
  if(!checkModule(m) && !checkEta(e) && !checkSub(s)) {
    softId = mNes*(m-1) + mNEta*(s-1) + e;
    return 0;
  }
  else {
    LOG_WARN << Form("<W> getId(2001 Aug Scheme) | Det %i bad index m %i e %i s %i ",mDetector,m,e,s) << endm; 
    return 1;
  }
}

inline Int_t 
StEmcGeom::getBin(const Int_t softId, Int_t &m, Int_t &e, Int_t &s) const
{
  static Int_t j, wid;
  if(!checkId(softId)) { 
    wid = softId - 1;  // from 0 to MAX-1
    m = wid/mNes + 1;
    j = wid - mNes*(m-1);
    s = j/mNEta  + 1;
    e = j%mNEta  + 1; 
    return 0;
  }
  else return 1;
}
#endif

#ifdef WSUJoseMarch
//
// March 2001  - now obsolete (7 Aug 2001)
//
inline Int_t 
StEmcGeom::getId(const Int_t m, const Int_t e, const Int_t s,Int_t &rid)
{
  if(!checkModule(m) && !checkEta(e) && !checkSub(s)){
    if(mDetector==1 || mDetector==2) { // only for bemc and bprs
      rid = 40*(m-1) + 20*(s-1) + (21-e);
    }
    if(mDetector==3 || mDetector==4) { // for BSMDE and BSDDP
      rid = mNes*(m-1) + mNEta*(s-1) + (mNEta - e) + 1;
    }
    return 0;
  }
  else {
    printf("<W> Det %i bad index m %i e %i s %i \n", mDetector, m, e, s); 
    return 1;
  }
}

inline Int_t 
StEmcGeom::getBin(const Int_t rid,Int_t &m,Int_t &e,Int_t &s)
{
  //
  // 15-mar-2001 for transition from environment (Jose) numeration to 
  // usual EMC'c numbering. This is standard now !!!
  // It is work only for BEMC and BPRS (15-mar-2001)
  //
  //  if(mDetector<1 || mDetector>3) {cout<<" Wrong number of detector "
  //                                    <<mDetector<<endl; return 1;}
  Int_t idw;
  if(mDetector==1 || mDetector==2) { // BEMC and BPRS
    if(checkId(rid) == 1) return 1;
    m   = (rid - 1) / 40 + 1; // Module number 
    idw = (rid - 1) % 40;     // change from 0 to 39
    s   = idw/20 + 1;
    e   = 20 - idw%20;
    return 0;                   // zero is good
  }
  else if(mDetector==3 || mDetector==4) { // BSMDE and BSMDP
    if(checkId(rid) == 1) return 1;
    m   = (rid - 1) / mNes + 1; // Module number 
    idw = (rid - 1) % mNes;     // change from 0 to mNes - 1
    s   = idw/mNEta + 1;
    e   = mNEta - idw%mNEta;
    return 0;                   // zero is good
  }
  else {
    cout<<" Wrong number of detector "<<mDetector<<endl; 
    return 1;
  }
}

#endif

#endif
