/***************************************************************************
 *
 * $Id: StEmcGeom.cxx,v 1.13 2018/12/20 22:07:05 perev Exp $
 *
 * Author: Aleksei Pavlinov , June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcGeom.cxx,v $
 * Revision 1.13  2018/12/20 22:07:05  perev
 * Remove creating and deleting StMaker
 *
 * Revision 1.12  2012/04/03 00:04:04  perev
 * Defence against zero size table added
 *
 * Revision 1.11  2008/11/03 21:00:37  mattheww
 * updated the geometry again
 *
 * Revision 1.10  2008/10/21 19:14:16  mattheww
 * Update to Barrel Geometry (corrected edge locations to eta = 0.0035, 0.9835)
 *
 * Revision 1.9  2008/04/22 12:24:52  kocolosk
 * bug was actually in the BSMDP mapping the whole time -- see RT #1162
 *
 * Revision 1.8  2008/04/16 20:57:05  kocolosk
 * rollback to 1.6 till we get RT#1162 ironed out
 *
 * Revision 1.7  2008/04/14 21:53:35  kocolosk
 * fix mapping between GEANT volume ID and m-e-s space for BTOW/BPRS, eta<0 (see RT# 1162)
 *
 * Revision 1.6  2007/04/04 17:32:11  kocolosk
 * Added softId-based versions of getEta, getTheta, and getPhi.  Also added getId(phi,eta,&softId).  Implemented const-correctness and used meaningful argument names in method declarations to improve readability
 *
 * Revision 1.5  2004/08/19 17:31:45  pavlinov
 * getBin(const Float_t phi, const Float_t eta, Int_t &m,Int_t &e,Int_t &s) works bsmde too - request of Dmitry Arkhipkin
 *
 * Revision 1.4  2003/09/02 17:58:01  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/04/16 15:27:51  suaide
 * small fix on default geant geometry
 *
 * Revision 1.2  2003/01/23 03:04:56  jeromel
 * Include modif
 *
 * Revision 1.1  2003/01/23 01:30:28  suaide
 * moving to sub directories
 *
 * Revision 1.15  2003/01/17 00:45:38  suaide
 * Default option for geometry is Y2003
 *
 * Revision 1.14  2001/09/28 23:54:37  pavlinov
 * Change dtor
 *
 * Revision 1.13  2001/09/22 00:29:05  pavlinov
 * No public constructor for StEmcGeom
 *
 * Revision 1.12  2001/07/30 00:16:07  pavlinov
 * Correct numbering scheme for BSMDE
 *
 * Revision 1.11  2001/05/30 18:01:22  perev
 * stdlib.h added
 *
 * Revision 1.10  2001/05/02 20:34:02  pavlinov
 * mCalg must zero on constractor
 *
 * Revision 1.9  2001/05/02 16:35:51  pavlinov
 * Change default value of calb for year2001 geometry
 *
 * Revision 1.8  2001/04/29 16:26:32  pavlinov
 * clean up
 *
 * Revision 1.7  2001/04/28 19:46:29  pavlinov
 * Reject output
 *
 * Revision 1.6  2001/04/26 14:23:40  akio
 * Quick and dirty fix for crashing non-bfc chain
 *
 * Revision 1.5  2001/04/25 02:41:45  pavlinov
 * Fixed erorr for transition from ivid to EMC numeration
 *
 * Revision 1.4  2001/04/25 01:03:03  pavlinov
 * Clean up
 *
 * Revision 1.3  2001/03/23 18:59:05  pavlinov
 * delete gROOT definition because exist in TROOT.h
 *
 * Revision 1.2  2001/03/22 21:50:40  pavlinov
 * Clean up for mdc4
 *
 * Revision 1.1  2000/06/20 17:11:25  pavlinov
 * Move StEmcGeom.h from St_emc_Maker to StEmcUtil
 *
 * Revision 1.10  2000/05/23 14:35:02  pavlinov
 * Clean up for SUN
 *
 * Revision 1.9  2000/05/18 17:07:31  pavlinov
 * Fixed error for methods getXYZ(...)
 *
 * Revision 1.8  2000/05/17 16:05:32  pavlinov
 * Change method getVolIdBemc
 *
 * Revision 1.7  2000/04/25 17:02:06  pavlinov
 * Added methods for gettinng x,y,z from volume ID
 *
 * Revision 1.6  2000/04/21 17:43:02  pavlinov
 * Added methods for for decoding Geant volume Id
 *
 * Revision 1.5  2000/04/18 20:38:10  pavlinov
 * Added ctor from Geant geometry
 *
 * Revision 1.4  2000/04/11 19:48:40  pavlinov
 * Merge versions of Pavlinov and Ogawa
 *
 * Revision 1.3  2000/01/29 00:04:57  akio
 * temprary fix for endcap. need more work, but no more junk messages and crash
 *
 * Revision 1.2  1999/07/02 03:01:55  pavlinov * Little corrections for Linux
 *
 * Revision 1.1  1999/07/01 16:17:57  pavlinov
 * class StEmcGeom was created and maker was remade for new maker scheme
 *
 **************************************************************************/
#include "StEmcGeom.h"
#include <assert.h>
#include <strings.h>
#include <stdlib.h>
#include <TROOT.h>
#include "StMaker.h"
#include "StEmcUtil/others/emcInternalDef.h"

ClassImp(StEmcGeom)

StEmcGeom *StEmcGeom::mGeom[8] = {0,0,0,0,0,0,0,0};

const Float_t perr=0.01;
Float_t rmin, rsmdEta, rsmdPhi;

StEmcGeom 
*StEmcGeom::instance(const Int_t det)
{
  return getEmcGeom(det);
}
StEmcGeom 
*StEmcGeom::getEmcGeom(const Int_t det)
{
  if(det>=1 && det<=4) {
    Int_t indDet = det - 1;
    if(mGeom[indDet] == 0) mGeom[indDet]  = new StEmcGeom(det);
    return mGeom[indDet];
  } else return 0; // wrong index
}


StEmcGeom 
*StEmcGeom::instance(const Char_t *cdet)
{
  return getEmcGeom(cdet);
}
StEmcGeom 
*StEmcGeom::getEmcGeom(const Char_t *cdet)
{
  Int_t det=getDetNumFromName(cdet);
  return getEmcGeom(det);
}


StEmcGeom 
*StEmcGeom::instance(const Int_t det, const Char_t* mode)
{
  return getEmcGeom(det,mode);
}
StEmcGeom 
*StEmcGeom::getEmcGeom(const Int_t det, const Char_t* mode)
{
  if(det>=1 && det<=4) {
    Int_t indDet = det - 1;
    if(mGeom[indDet] == 0) mGeom[indDet]  = new StEmcGeom(det, mode);
    return mGeom[indDet];
  } else return 0; // wrong index
}

//===============================================
StEmcGeom::StEmcGeom(const Int_t det) 
{
  initGeom(det);
}

StEmcGeom::StEmcGeom(const Char_t *cdet) 
{
  Int_t det=getDetNumFromName(cdet);
  if(det) initGeom(det);
}

Int_t 
StEmcGeom::getDetNumFromName(const Char_t *cdet) 
{
  Int_t det=0;
  if     (!strcmp(cdet,"bemc")) {det=1;}
  else if(!strcmp(cdet,"bprs")) {det=2;}
  else if(!strcmp(cdet,"bsmde")){det=3;}
  else if(!strcmp(cdet,"bsmdp")){det=4;}
  else if(!strcmp(cdet,"eemc")) {det=5;}
  else if(!strcmp(cdet,"eprs")) {det=6;}
  else if(!strcmp(cdet,"esmde")){det=7;}
  else if(!strcmp(cdet,"esmdp")){det=8;}
  else {LOG_ERROR << Form(" StEmcGeom: Bad value of cdet %s ", cdet) << endm;}
  return det;
}

StEmcGeom::~StEmcGeom() 
{// for shure
  mGeom[mDetector] = 0;
} 

void StEmcGeom::initGeom(const Int_t det) 
{
  mMode="default";
  mDetector = det;
  mGeantGeom = 0;
  mCalg    = 0;
  mCalg_st = 0;
  mCalr    = 0;
  mCalr_st = 0;

  mPhiOffset.Set(2); mPhiStep.Set(2); mPhiBound.Set(2);

  defineDefaultCommonConstants();
  defineModuleGridOnPhi();

  mMaxAdc = 1024; if(det==1) mMaxAdc = 4096;
  //  getGeantGeometryTable();

  switch (det){
  case 1:
  case 2:
    mRadius   = 225.405;   // Edge of SC1 (223.5+2* 0.9525)
    mYWidth   =  11.174;   // Was 11.1716 before 18-apr-2000;  
    initBEMCorBPRS();
    break;
  case 3:
    mRadius   = 230.705;   // Was 230.467 before 18-apr-2000;   
    mYWidth   = 11.2014*2;  
    initBSMDE();
    break;
  case 4:
    mRadius   = 232.742;   // Was 232.467 before 18-apr-2000;
    mYWidth   = 22.835;    // From UCLA drawing
    initBSMDP();
    break;
  case 5:
  case 6:
    initEEMCorEPRS();
    break;
  case 7:
    initESMDE();
    break;
  case 8:
    initESMDP();
    break;
  default:
    LOG_FATAL << Form(" StEmcGeom: Bad value of mDetector %i ", mDetector) << endm;
    assert(0);
  }
  mGeom[det-1] = this; // 19-aug-04
}
// _____________________________________________________________________
StEmcGeom::StEmcGeom(const Int_t det, const Char_t *mode) 
{
  mMode=mode; mMode.ToLower();
  if(mMode == "geant"){ // Compare without case 
    getGeantGeometryTable();
  }
  else mMode.Append(" : wrong option !!! "); 

  if(!(mMode=="geant")){
    LOG_WARN << Form("<W> Something wrong(%s)=> using default",mMode.Data()) << endm;
    initGeom(det); 
    return;
  }

  LOG_INFO << Form("<I> Used Geant Geometry for BEMC, version %5.2f ",mCalg_st->version) << endm;
  mDetector = det;
  mPhiOffset.Set(2); mPhiStep.Set(2); mPhiBound.Set(2);

  defineCommonConstants();
  defineModuleGridOnPhi();

  Float_t currentDepth;
  switch (det){
  case 1:
  case 2:
    mRadius   = rmin;
  // see CSCI in geometry.g 
    currentDepth = mRadius+mCalg_st->scintthk[0]+2.*mCalg_st->abpapthk;
    mYWidth   = currentDepth*tan(mPhiStepHalf)-mCalg_st->crackwd;
    initBEMCorBPRS();
    break;
  case 3:
    mRadius   = rsmdEta;
    mYWidth   = mCalg_st->smalfwdh*2.;
    initBSMDE();
    break;
  case 4:
    mRadius   = rsmdPhi;
    mYWidth   = mCalg_st->smalfwdh*2.;
    initBSMDP();
    break;
  case 5:
  case 6:
    initEEMCorEPRS();
    break;
  case 7:
    initESMDE();
    break;
  case 8:
    initESMDP();
    break;
  default:
    LOG_ERROR << Form(" StEmcGeom: Bad value of mDetector %i ", mDetector) << endm;
  }
}

void 
StEmcGeom::defineDefaultCommonConstants()
{
  // Common information for all detectors
  mNModule  = 120;
  mEtaMax   = 0.984;
  mEtaMin   = 0.0035;

  mPhiOffset[0] = (75.-3.)/ 180. * C_PI;
  mPhiOffset[1] = (105.+3.)/180. * C_PI;
  mPhiStep[0]   = -C_2PI /(mNModule/2); 
  mPhiStep[1]   = -mPhiStep[0];

  mPhiBound[0] = 75. /180. * C_PI;
  mPhiBound[1] = 105./180. * C_PI;
  mPhiStepHalf = 3. * C_PI/180.;
}

void 
StEmcGeom::defineCommonConstants()
{
  Float_t lW[2], smdW;
  mNModule      = 120;  // mCalg_st->maxmodul;
  mEtaMax       = 0.984; // mCalg_st->etacut;  ?? Why 1.0 in geometry
  mEtaMin       = 0.0035;

  mPhiStepHalf  = 360. / (Float_t)mNModule; // in degree

  //
  // 24-apr 2001 because mCalg_st->shift can change in Geant !!!
  //
  mPhiOffset[0] = (75.-3.)/ 180. * C_PI;
  mPhiOffset[1] = (105.+3.)/180. * C_PI;

  mPhiStep[0]   = -toRad(2.*mPhiStepHalf); 
  mPhiStep[1]   = -mPhiStep[0];
  mPhiStepHalf  = toRad(mPhiStepHalf);
  
  rmin = mCalr_st->rmin;
  Int_t nsuper=(Int_t)mCalg_st->nsuper;
  Int_t nsmd=(Int_t)mCalg_st->nsmd;
  for(Int_t i=0; i<2; i++){
    lW[i] = mCalg_st->scintthk[i]+mCalg_st->absorthk+2.*mCalg_st->abpapthk;
  }
  // Radius of begin of SMD (Eta plane)
  rsmdEta=rmin+2.*(lW[0]*nsuper+lW[1]*(nsmd-nsuper));
  // Half width of SMD 
  smdW=2.*(mCalg_st->g10sbthk+mCalg_st->smalfthk+mCalg_st->abpapthk);
  // Radius of end of SMD (Phi plane)
  rsmdPhi=rsmdEta + 2.*smdW;
}

void 
StEmcGeom::defineModuleGridOnPhi()
{
  //
  // -pi <= phi < pi
  //
  Int_t   mw,ew,sw;
  Float_t etaw=-0.1;
  mPhiModule.Set(mNModule);
  Int_t im = 0;
  for(Int_t i=0; i<mNModule/2; i++){
    //    Int_t im = 2*i/mNModule;
    Double_t phiW = mPhiOffset[im] + mPhiStep[im]*i;
    while(phiW >= C_PI) phiW -= C_2PI;
    while(phiW < -C_PI) phiW += C_2PI;
    if(phiW > (C_PI-0.0001)) phiW = -C_PI; // -pi<=phi<phi
    mPhiModule[i] = phiW;
    Int_t cond = getBin(mPhiModule[i], etaw, mw,ew,sw);
    if   (!cond) mPhiModule[mw-1] = phiW; // Second barrel
    else {LOG_WARN << "<W> Something wrong in StEmcGeom::defineModuleGridOnPhi()" << endm;}
  }
}

void 
StEmcGeom::initBEMCorBPRS() 
{
  if(mMode.CompareTo("geant") == 0) {
    mNEta = (Int_t)mCalg_st->netat;
    mNSub = (Int_t)mCalg_st->nsub;
  }
  else {mNEta = 20; mNSub= 2;}

  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  mEtaB.Set(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {mEtaB[i] = 0.05*i;} mEtaB[mNEta]=mEtaMax; mEtaB[0]=mEtaMin;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (mEtaB[i+1] + mEtaB[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)
  // mYlocal, mPhi, mYB and mPhiB - have the same sign ( 29-jul-2001) !!!
  mYlocal.Set(mNSub);  
  mYlocal[0] =  -mYWidth/2.;    mYlocal[1] = mYWidth/2.;  

  mPhi.Set(mNSub); 
  for(Int_t i=0;i<mNSub; i++) mPhi[i] =  atan2(mYlocal[i],mRadius);

  mYB.Set(mNSub+1);   mPhiB.Set(mNSub+1);   // 28-jul-2001
  mYB[0] = -mYWidth;
  mYB[1] = 0.0;
  mYB[2] = +mYWidth;
  for(Int_t i=0; i<mNSub+1; i++) mPhiB[i] =  atan2(mYB[i],mRadius);

}
// _____________________________________________________________________
void StEmcGeom::initBSMDE(){
  Float_t smetawdh, seta1wdh, seta2wdh, seta12wdh; // Size for eta strips
  if(mMode.CompareTo("geant") == 0) {
    mNEta = (Int_t)mCalg_st->netfirst + (Int_t)mCalg_st->netsecon;
    mNSub = 1;
    smetawdh = mCalg_st->smetawdh;
    seta1wdh = mCalg_st->seta1wdh; 
    seta2wdh = mCalg_st->seta2wdh; 
    seta12wdh= mCalg_st->set12wdh;
  }
  else {
    mNEta = 150;
    mNSub = 1;
    smetawdh=0.9806;
    seta1wdh=0.7277; 
    seta2wdh=0.9398; 
    seta12wdh=0.0406;
  }

  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub);
  mEtaB.Set(mNEta+1);  // Eta boundaries 27-jul-2001
  TArrayF zb(mNEta+1); // z   boundaries
  
  // Eta variable ( Z direction)
  Int_t i; 
  Float_t shift1, shift2;
  shift1 = 2.*smetawdh + seta1wdh;    // The center of first eta strip
  //shift1 = (1.18-0.032/2-0.573/2)*2.54;   // From drawing of SMD
  shift2 = shift1 + (seta1wdh+seta12wdh)*2*74 + (seta1wdh+seta12wdh)
                  + (seta2wdh+seta12wdh); // The center of 76h eta strip
  zb[0] = 2.*smetawdh;
  for(i=0; i<mNEta; i++) {
    if(i<mNEta/2) {
      mZlocal[i] = shift1 + (seta1wdh+seta12wdh)*2*i;
      zb[i+1]    = zb[i]  + (seta1wdh+seta12wdh)*2;
    }
    else {
      mZlocal[i] = shift2 + (seta2wdh+seta12wdh)*2*(i-75);
      zb[i+1]    = zb[i]  + (seta2wdh+seta12wdh)*2;
    }
    mEta[i]  = -::log(tan(atan2(mRadius,mZlocal[i])/2.0));
    mEtaB[i] = -::log(tan(atan2(mRadius,zb[i])/2.0));
  }
  mEtaB[mNEta] = -::log(tan(atan2(mRadius,zb[mNEta])/2.0));
  // 19-aug-2004 ; request of Dmitry Arkhipkin
  mEtaMin = mEtaB[0];
  mEtaMax = mEtaB[mNEta];

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub); mYlocal[0] = 0.0;
  mPhi.Set(mNSub);    mPhi[0]    = 0.0;

  mYB.Set(mNSub+1);   mPhiB.Set(mNSub+1); // 29-jul-2009
  mYB[0] = -mYWidth/2.;
  mYB[1] =  mYWidth/2.;
  for(Int_t i=0; i<mNSub+1; i++) mPhiB[i] =  atan2(mYB[i],mRadius);
}
// _____________________________________________________________________
void StEmcGeom::initBSMDP()
{
  Float_t sphiwdh, sphidwdh, shift, smdgaswdh;
 
  if(mMode.CompareTo("geant") == 0) {
    mNEta    = (Int_t)mCalg_st->netasmdp;
    mNSub    = (Int_t)mCalg_st->nphistr;
    sphiwdh  = mCalg_st->sphiwdh;
    sphidwdh = mCalg_st->sphidwdh;
    smdgaswdh= mCalg_st->smgaswdh;
    
  }
  else{
    mNEta     = 10; 
    mNSub     = 15;
    sphiwdh   = 0.668;    // half width for phi strips
    sphidwdh  = 0.07874;  // half distance between strips in phi
    smdgaswdh = 0.295;    // smd gas box volume half width
  }
  shift = - mYWidth/2. + smdgaswdh + sphiwdh; // The position of center first phi strip

  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  mEtaB.Set(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {mEtaB[i] = 0.1*i;} 
  mEtaB[mNEta]=mEtaMax;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (mEtaB[i+1] + mEtaB[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)

  mYlocal.Set(mNSub); mPhi.Set(mNSub);
  mYB.Set(mNSub+1);   mPhiB.Set(mNSub+1); // 27-jul-2001
  mYB[0] = -mYWidth/2. + smdgaswdh;

  if(mMode.CompareTo("geant") == 0){ // For odd numbers ( default 15) ?? must check
    Int_t n2=mNSub/2;
    mYlocal[n2] = 0.0;
    for(i=n2+1; i<mNSub; i++){
      mYlocal[i] = (sphiwdh+sphidwdh)*2*(i-n2);
      mYlocal[mNSub-i-1] = -mYlocal[i];
    }
    for(i=0; i<mNSub; i++){
      mPhi[i] = atan2(mYlocal[i], mRadius);
    }
  }
  else{
    for(i=0; i<mNSub; i++){
      mYlocal[i] =  shift  + (sphiwdh+sphidwdh)*2*i;
      mPhi[i]    =  atan2(mYlocal[i], mRadius);
      if(i==0 || i==(mNSub-1)) mYB[i+1] = mYB[i] + sphiwdh*2. + sphidwdh;
      else                     mYB[i+1] = mYB[i] + (sphiwdh + sphidwdh)*2.;
      mPhiB[i] =  atan2(mYB[i], mRadius);
    }
    mPhiB[mNSub] = atan2(mYB[mNSub], mRadius);
  }
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolIdBemc(const Int_t ivid, Int_t &module,Int_t &eta,Int_t &sub, Int_t &detector)
{
  // Transition from Geant Volume Id to usual for BEMC and BPRS
  // See  emc/util/volid_bemc.F

  static Int_t emcIvid[5]={10000000,100000,100,10,1};
  Int_t emcChid[5], i, ividw, rl, phi, dep;
  //  assert(mCalg_st); // 24-apr
  if(mCalg_st == 0) getGeantGeometryTable();

  ividw = ivid;
  for(i=0; i<5; i++){
    emcChid[i] = ividw/emcIvid[i];
    ividw      = ividw%emcIvid[i];
  }
  if(ividw == 0){
    rl     = emcChid[0];  // right/left: =1 for Z>0, and =2 for Z<0
    eta    = emcChid[1];  // pseudorapidity bin number [1,20]
    phi    = emcChid[2];  // module phi [1,120]
    sub    = emcChid[3];  // d(eta)=0.1 tower number [1,2]
    dep    = emcChid[4];  // depth section [1,2];
    switch (dep) {// see ems_interface2.F
    case 1: 
      detector = BPRS; break;
    case 2: 
      detector = BEMC; break;
    default:
      LOG_WARN << Form("<W> StEmcGeom::getVolIdBemc => wrong value of dep %i ",dep) << endm;
    }
    if     (rl==1) {
      //      cout<<" Phi 1 "<<phi<<endl;
      //phi += Int_t((mCalg_st->shift[0]-75.)/6.);
      phi += Int_t((75.-mCalg_st->shift[0])/6.);
      while (phi<=0)  phi+=60;
      while (phi>=61) phi-=60;
      module=phi;
      //      cout<<" Phi 2 "<<phi<<endl;
    }
    else if(rl==2) {
      //      phi += Int_t((shift[1]-105.)/6.);
      phi += Int_t((105.-mCalg_st->shift[1])/6.);
      while (phi<=0)  phi+=60;
      while (phi>=61) phi-=60;
      module=phi+60;
      sub   =(sub+1)%2+1;
    }
    else{
    LOG_ERROR << Form("<E> getVolIdBemc -- error decoding BEMC Geant volume Id %i; rl=%i", ivid, rl) << endm;
    return 1;
    }
  }
  else {
    LOG_ERROR << Form("<E> getVolIdBemc -- error decoding BEMC Geant volume Id %i=>%i", ivid, ividw) << endm;
    return 1;
  }
  //  printf(" vid %i m %3i eta %3i sub %3i dep %3i \n",
  // ivid,module,eta,sub,dep);  
  return 0;
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolIdBsmd(const Int_t ivid, Int_t &module,Int_t &eta,Int_t &sub, Int_t &detector)
{
  // Transition from Geant Volume Id to usual for BSMDE and BSMDP
  // See  emc/util/volid_bsmd.F
  static Int_t smdIvid[5]={100000000,1000000,1000,100,1}; //matched with AGI&G2T
  Int_t smdChid[5], i, ividw, rl, phi, t, strip;
  //  assert(mCalg_st); // 24-apr
  if(mCalg_st == 0) getGeantGeometryTable();

  ividw = ivid;
  for(i=0; i<5; i++){
    smdChid[i] = ividw/smdIvid[i];
    ividw      = ividw%smdIvid[i];
  }
  if(ividw == 0){
    rl     = smdChid[0];  // right/left: =1 for Z>0, and =2 for Z<0
    eta    = smdChid[1];  // pseudorapidity bin number [-10,10]
    phi    = smdChid[2];  // module phi [1,60]
    t      = smdChid[3];  // SMD type 1->3
    strip  = smdChid[4];  // strip number 1-75(type 1,2) 1-15(type 3)
    if     (rl==1) {
      //      phi += Int_t((mCalg_st->shift[0]-75.)/6.);
      phi += Int_t((75. - mCalg_st->shift[0])/6.);
      while (phi<=0)  phi+=60;
      while (phi>=61) phi-=60;
      module=phi;
    }
    else if(rl==2) {
      //      phi += Int_t((mCalg_st->shift[1]-105.)/6.);
      phi += Int_t((105. - mCalg_st->shift[1])/6.);
      while (phi<=0)  phi+=60;
      while (phi>=61) phi-=60;
      module=phi+60;
    }
    else{
      LOG_ERROR << Form("<E> getVolIdBsmd -- error decoding BSMD Geant volume Id %i; rl=%i", ivid, rl) << endm;
      return 1;
    }
    if     (t==1){
      detector = BSMDE;
      eta  = strip;
      sub  = 1;
    }
    else if(t==2){
      detector = BSMDE;
      eta  = strip + 75;
      sub  = 1;
    }
    else if(t==3){
      detector = BSMDP;
      eta  = abs(eta);
      
      // SMDP West: sub = 16 - strip
      // SMDP East: sub  = strip
      switch (rl) {
        case 1:
          sub = 16 - strip;
          break;
        case 2:
          sub = strip;
          break;
      }
    }
    else {
		LOG_ERROR << Form("<E> getVolIdBsmd: Type mismatch %i ",t) << endm;
      return 1;
    }
  }
  else {
    LOG_ERROR << Form("<E> getVolIdBsmd -- error decoding BSMD Geant volume Id %i=>%i", ivid, ividw) << endm;
    return 1;
  }
  //  printf(" vid %i m %3i eta %3i sub %3i type %3i \n",
  //ivid,module,eta,sub,type);  
  return 0;
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolId(const Int_t ivid, Int_t &module,Int_t &eta,Int_t &sub, Int_t &det)
{
  if     (mDetector==1||mDetector==2) return getVolIdBemc(ivid,module,eta,sub,det);
  else if(mDetector==3||mDetector==4) return getVolIdBsmd(ivid,module,eta,sub,det);
  else {
    LOG_ERROR << Form("<E> getVolId -- wrong detectot number %i ",mDetector) << endm;
    return 0;
  }
}
// _____________________________________________________________________
void StEmcGeom::initEEMCorEPRS()  //wrong need to update
{
  mNModule  = 24;
  mNEta     = 12;  
  mNSub     = 5;  
  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mRadius   = 225.405;  // Edge of SC1 (223.5+2* 0.9525)
  mYWidth   = 11.1716;  
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  mEtaB.Set(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {mEtaB[i] = 0.05*i;} mEtaB[mNEta]=0.99;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (mEtaB[i+1] + mEtaB[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub);  
  mYlocal[0] =   mYWidth/2;    mYlocal[1] = - mYlocal[0];  

  mPhi.Set(mNSub); 
  mPhi[0] =  atan2(mYWidth/2,mRadius);    mPhi[1] = -mPhi[0];

  //  cout<<" Default constructor for StEmcGeom (Ver. 1.00 # 20-Jun-1999 )"<<endl;
}
// _____________________________________________________________________
void StEmcGeom::initESMDE(){  //wrong need to update
  mNModule  = 24;
  mNEta     = 200;  
  mNSub     = 200;  
  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mRadius   = 230.467;   // See find_pos_ems.F or Geant Geometry
  mYWidth   = 11.2014*2;  
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  Int_t i; 
  Float_t smetawdh=0.9806;
  Float_t seta1wdh=0.7277, seta2wdh=0.9398, seta12wdh=0.0406; // Size for eta strips
  Float_t shift1, shift2;
  shift1 = 2.*smetawdh + seta1wdh;    // The center of first eta strip
  //shift1 = (1.18-0.032/2-0.573/2)*2.54;   // From drawing of SMD
  shift2 = shift1 + (seta1wdh+seta12wdh)*2*74 + (seta1wdh+seta12wdh)
                  + (seta2wdh+seta12wdh); // The center of 76h eta strip

  for(i=0; i<mNEta; i++) {
    if(i<mNEta/2) mZlocal[i] = shift1 + (seta1wdh+seta12wdh)*2*i;
    else mZlocal[i] = shift2 + (seta2wdh+seta12wdh)*2*(i-75);
    mEta[i] = -::log(tan(atan2(mRadius,mZlocal[i])/2.0));
  }

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub); mYlocal[0] = 0.0;

  mPhi.Set(mNSub); mPhi[0] =  0.0;

  //  cout<<" (BSMDE) shift1 "<<shift1<<endl;
  //cout<<" (BSMDE) shift2 "<<shift2<<endl;
}
// _____________________________________________________________________
void StEmcGeom::initESMDP()  //wrong Need to update
{
  mNModule  = 24;
  mNEta     = 200;  
  mNSub     = 200;  
  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mRadius   = 232.467;   // See find_pos_ems.F or Geant Geometry
  mYWidth   = 22.835;  
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  mEtaB.Set(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {mEtaB[i] = 0.1*i;} 
  mEtaB[mNEta]=0.99;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (mEtaB[i+1] + mEtaB[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)

  mYlocal.Set(mNSub); mPhi.Set(mNSub);

  Float_t sphiwdh  = 0.668;           // half width for phi strips
  Float_t sphidwdh = 0.07874;         // half distance between strips in phi
  Float_t shift    = mYWidth/2. - 0.295 - sphiwdh; // The position of center first phi strip

  for(i=0; i<mNSub; i++){
    mYlocal[i] = shift - (sphiwdh+sphidwdh)*2*i;
    mPhi[i] = atan2(mYlocal[i], mRadius);
  }
}
// _____________________________________________________________________
void  StEmcGeom::printGeom() const
{
  cout<<" mMode      "<<mMode.Data()<<endl;
  cout<<" mDetector  "<<mDetector<<endl;
  cout<<" mNModule   "<<mNModule<<endl;
  cout<<" mNEta      "<<mNEta<<endl;
  cout<<" mNSub      "<<mNSub<<endl;
  cout<<" mNes       "<<mNes<<endl;
  cout<<" mNRaw      "<<mNRaw<<endl;
  cout<<" mRadius    "<<mRadius<<endl;
  cout<<" mYWidth    "<<mYWidth<<endl;
  cout<<" mEtaMin    "<<mEtaMin<<endl;
  cout<<" mEtaMax    "<<mEtaMax<<endl;
  cout<<" mPhiOffset "<<mPhiOffset[0]<<"("<<toDeg(mPhiOffset[0])<<")   "
      <<mPhiOffset[1]<<"("<<toDeg(mPhiOffset[0])<<")"<<endl;
  cout<<" mPhiStep   "<<mPhiStep[0]<<"("<<toDeg(mPhiStep[0])<<")   "
      <<mPhiStep[1]<<"("<<toDeg(mPhiStep[1])<<")"<<endl;
  cout<<" Max ADC    "<<mMaxAdc<<endl;

  Int_t i;
  cout<<"\n Z grid and Eta grid "<<endl;
  cout<<" mEtaB[0] "<<mEtaB[0]<<endl;
  for(i=0; i<mNEta; i++){
    printf(" i %3i  Zl %7.3f Eta %8.5f mEtaB %7.5f\n" 
    , i, mZlocal[i], mEta[i], mEtaB[i+1]); 
  }

  cout<<"\n Y grid and Phi grid "<<endl;
  //  cout<<"mYB[0] "<<mYB[0]<<" mPhiB[0] "<<mPhiB[0]<<endl;
  printf(" mYB %7.3f mPhiB %9.5f \n", mYB[0], mPhiB[0]);
  for(i=0; i<mNSub; i++){
    printf(" i %2i Yl %7.3f Phi %9.5f ", i, mYlocal[i], mPhi[i]);
    printf(" mYB %7.3f mPhiB %9.5f \n", mYB[i+1], mPhiB[i+1]);
  }
  cout<<"\n   Phi grid of center of modules in STAR system\n";
  cout<<"   =============================================\n";
  Int_t   mw,ew,sw;
  Float_t etaw=-0.1;
  for(i=0; i<mNModule/2; i++){
    printf(" %3i phi %10.7f (%6.1f)",      // First  barrel
    i+1,mPhiModule[i],mPhiModule[i]*C_DEG_PER_RAD);
    Int_t cond = getBin(mPhiModule[i], etaw, mw,ew,sw);
    if(!cond) {
      printf(" => %3i phi %10.7f (%6.1f)", // Second barrel
      mw,mPhiModule[mw-1],mPhiModule[mw-1]*C_DEG_PER_RAD);
    }
    printf("\n");
  }
  cout<<"\n == "<<endl; 
  for(Int_t i=0;i<4; i++){
    cout<<" Pointer for det = "<<i+1<<" -> "<<mGeom[i]<<endl;
  }
}
// _____________________________________________________________________
void  StEmcGeom::compare(const StEmcGeom &g, Bool_t key=kFALSE) const
{
  Float_t err;
  Int_t i;
  if(mDetector==g.Detector()) {
    printf(" mMode      %10s | %10s \n", mMode.Data(), g.Mode()->Data());
    printf("---------------------------------------------------\n");
    if(mNModule != g.NModule() || key) 
    printf(" mNModule   %10i | %10i\n",mNModule,g.NModule());
    if(mNEta != g.NEta() || key)
    printf(" mNEta      %10i | %10i\n",mNEta,g.NEta());
    if(mNSub != g.NSub() || key)
    printf(" mNSub      %10i | %10i\n",mNSub,g.NSub());
    if(mNes != g.Nes() || key)
    printf(" mNes       %10i | %10i\n",mNes,g.Nes());
    if(mNRaw != g.NRaw() || key)
    printf(" mNRaw      %10i | %10i\n",mNRaw,g.NRaw());

    err=relativeError(mRadius,g.Radius());
    if(err>perr || key){ 
      printf(" mRadius    %10.3f | %10.3f",mRadius,g.Radius());
      printError(err);
    }

    err=relativeError(mYWidth,g.YWidth());
    if(err>perr || key) {
      printf(" mYWidth    %10.3f | %10.3f",mYWidth,g.YWidth());
      printError(err);
    }

    err=relativeError(mEtaMax,g.EtaMax());
    if(err>perr || key) {
      printf(" mEtaMax    %10.3f | %10.3f",mEtaMax,g.EtaMax());
      printError(err);
    }

    for(i=0; i<2; i++){
      err=relativeError(mPhiOffset[i],g.PhiOffset()[i]);
      if(err>perr || key){
        printf(" mPhiOffset[%1i] %7.3f | %10.3f",i,mPhiOffset[i],g.PhiOffset()[i]);
        printError(err);
      }
    }
    for(i=0; i<2; i++){
      err=relativeError(mPhiStep[i],g.PhiStep()[i]);
      if(err>perr || key){
        printf(" mPhiStep[%1i]   %7.3f | %10.3f",i,mPhiStep[i],g.PhiStep()[i]);
        printError(err);
      }
    }
    printf("\n Phi grid for center of module in STAR system mNModule=%i\n",mNModule);
    for(i=0; i<mNModule; i++){
      err=relativeError(mPhiModule[i],g.PhiModule()[i]);
      if(err>perr || key){
        printf(" %3i phi %7.2f | %10.2f",i,toDeg(mPhiModule[i])
        ,toDeg(g.PhiModule()[i]));
        printError(err);
      }
    }

    printf("\n   Z grid       and      Eta grid => mNEta=%i\n",mNEta);
    for(i=0; i<mNEta; i++){
      err=relativeError(mZlocal[i], g.Zlocal()[i]);
      if(err>perr || key){
        printf(" %3i  Zl %7.2f | %10.2f   || ",i, mZlocal[i], g.Zlocal()[i]);
        printf("Eta %7.3f | %7.3f",mEta[i], g.Eta()[i]);
        printError(err);
      }
    }

    printf("\n   Y grid       and      Phi grid => mNSub=%i\n",mNSub);
    for(i=0; i<mNSub; i++){
      err=relativeError(mYlocal[i], g.Ylocal()[i]);
      if(err>perr || key){
        printf("%3i Yl %7.3f | %10.3f   || ",i, mYlocal[i], g.Ylocal()[i]);
        printf("Phi %9.6f | %9.6f",mPhi[i], g.Phi()[i]);
        printError(err);
      }
    }
  }
  else printf("<W> You compare geometry for different detector %i != %i \n",
  mDetector, g.mDetector);
}
// _____________________________________________________________________
Float_t  StEmcGeom::relativeError(Float_t a, Float_t b) const
{
  Float_t sum = fabs(a) + fabs(b), perr;
  if(sum == 0.0) return 0.0; // Both zero
  else {
    perr = 200.*fabs(a-b)/sum;
    return perr;
  }
}

void  
StEmcGeom::printError(Float_t err) const
{
  	if(err>perr) {LOG_INFO << Form(" | perr=%6.3f%% ",err) << endm;}
	else {LOG_INFO << " " << endm; }
}

void 
StEmcGeom::getGeantGeometryTable()
{
// 24-apr-2000 for MDC4
// Will be work if BFC has name "bfc" !!! Be carefull
	mGeantGeom = NULL;
	mCalg = 0;
	mCalr = 0;
	mCalg_st = 0;
	mCalr_st = 0;
	mChain = StMaker::GetChain();

	if(mChain) mGeantGeom = mChain->GetDataSet(".const/geom");

	TString line;
	
	if(mGeantGeom) {
		mCalg    = (St_calb_calg   *) mGeantGeom->Find("calb_calg");
		if(mCalg && mCalg->GetNRows()) {
			mCalg_st = mCalg->GetTable();
			printf("calb_calr get from Geant::"); 
			for(Int_t i=0;i<2;i++) printf(" Barrel %i Angle shift %6.0f ", i+1, mCalg_st->shift[i]);
			printf("\n");
			mCalr = (St_calb_calr   *) mGeantGeom->Find("calb_calr");
			if(mCalr) mCalr_st = mCalr->GetTable(); // BARREL EMC RADIUSES
		}
	}
	if(!mCalg_st || !mCalr_st) {
		mMode.Append(" : No table");
		LOG_INFO << Form("StEmcGeom::getGeantGeometryTable() could not find geom") << endm;
		LOG_INFO << Form("StEmcGeom::getGeantGeometryTable() create own calb_calg/r") << endm;
		mCalg = new St_calb_calg("calg", 1);
		mCalr = new St_calb_calr("calr", 1);
		mCalg_st = mCalg->GetTable();
		mCalr_st = mCalr->GetTable();
	// For year2001 configuration only
	//mCalg_st[0].shift[0]=21.0;
	//mCalg_st[0].shift[1]=0.0;
	// For year2003 configuration only
		mCalg_st[0].shift[0]=75.0;
		mCalg_st[0].shift[1]=105.0;
	}
}
