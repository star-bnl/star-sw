/***************************************************************************
 *
 * $Id: StEmcGeom.cxx,v 1.9 2000/05/18 17:07:31 pavlinov Exp $
 *
 * Author: Aleksei Pavlinov , June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcGeom.cxx,v $
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
#include <strings.h>
#include "StEmcGeom.h"
#include "TROOT.h"
#include "emc_def.h"

ClassImp(StEmcGeom)

const Float_t perr=0.01;
Float_t rmin, rsmdEta, rsmdPhi;

// _____________________________________________________________________
StEmcGeom::StEmcGeom(const Int_t det) 
{
  initGeom(det);
}
// _____________________________________________________________________
StEmcGeom::StEmcGeom(const Char_t *cdet) 
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
  else {printf(" StEmcGeom: Bad value of cdet %s \n", cdet);}

  if(det) initGeom(det);
}
// _____________________________________________________________________
StEmcGeom::~StEmcGeom() { /* Nobody */ } 
// _____________________________________________________________________
void StEmcGeom::initGeom(const Int_t det) 
{
  mMode="default";
  mDetector = det;

  mPhiOffset.Set(2); mPhiStep.Set(2); mPhiBound.Set(2);

  defineDefaultCommonConstants();
  defineModuleGridOnPhi();

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
    printf(" StEmcGeom: Bad value of mDtector %i \n", mDetector);
  }
}
// _____________________________________________________________________
StEmcGeom::StEmcGeom(const Int_t det, const Char_t *mode) 
{
  mMode=mode; mMode.ToLower();
  if(mMode == "geant"){ // Compare without case 
    // Will be work if BFC has name "bfc" !!! Be carefull
    TList *tl = (TList*)gROOT->GetListOfBrowsables();
    if(tl) {mChain=(StBFChain*)tl->FindObject("bfc"); 
      if(mChain){mGeom = mChain->GetDataSet("geom");
      }
    }
    if(mGeom == 0) {mCalg = 0;mCalr = 0;}
    else{
      mCalg    = (St_calb_calg   *) mGeom->Find("calb_calg");
      mCalg_st = mCalg->GetTable();
      mCalr = (St_calb_calr   *) mGeom->Find("calb_calr");
      mCalr_st = mCalr->GetTable();
      if(!mCalg_st && !mCalr_st) mMode.Append(" : No table");
    }
  }
  else mMode.Append(" : wrong option !!! "); 

  if(!(mMode=="geant")){
    printf("<W> Something wrong(%s)=> using default\n",mMode.Data());
    initGeom(det); 
    return;
  }

  printf("<I> Used Geant Geometry for BEMC, version %5.2f \n",mCalg_st->version);
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
    printf(" StEmcGeom: Bad value of mDetector %i \n", mDetector);
  }
}
// _____________________________________________________________________
void StEmcGeom::defineDefaultCommonConstants()
{
  // Common information for all detectors
  mNModule  = 120;
  mEtaMax   = 0.99;

  mPhiOffset[0] = (75.-3.)/ 180. * C_PI;
  mPhiOffset[1] = (105.+3.)/180. * C_PI;
  mPhiStep[0]   = -C_2PI /(mNModule/2); 
  mPhiStep[1]   = -mPhiStep[0];

  mPhiBound[0] = 75. /180. * C_PI;
  mPhiBound[1] = 105./180. * C_PI;
  mPhiStepHalf = 3. * C_PI/180.;

}
// _____________________________________________________________________
void StEmcGeom::defineCommonConstants()
{
  Float_t lW[2], smdW;
  mNModule      = 120;  // mCalg_st->maxmodul;
  mEtaMax       = 0.99; // mCalg_st->etacut;  ?? Why 1.0 in geometry

  mPhiStepHalf  = 360. / (Float_t)mNModule; // in degree

  mPhiOffset[0] = toRad(mCalg_st->shift[0] - mPhiStepHalf);
  mPhiOffset[1] = toRad(mCalg_st->shift[1] + mPhiStepHalf);
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
// _____________________________________________________________________
void StEmcGeom::defineModuleGridOnPhi()
{
  mPhiModule.Set(mNModule);
  for(Int_t i=0; i<mNModule; i++){
    Int_t im = 2*i/mNModule;
    Float_t phiW=mPhiOffset[im]+mPhiStep[im]*i;
    while(phiW >=  C_PI) phiW -= C_2PI;
    while(phiW <  -C_PI) phiW += C_2PI;
    mPhiModule[i] = phiW;
  }
}
// _____________________________________________________________________
void StEmcGeom::initBEMCorBPRS() 
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
  TArrayF etaw(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {etaw[i] = 0.05*i;} etaw[mNEta]=mEtaMax;;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (etaw[i+1] + etaw[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub);  
  mYlocal[0] =   mYWidth/2.;    mYlocal[1] = - mYlocal[0];  

  mPhi.Set(mNSub); 
  mPhi[0] =  atan2(mYWidth/2.,mRadius);    mPhi[1] = -mPhi[0];

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
  
  // Eta variable ( Z direction)
  Int_t i; 
  Float_t shift1, shift2;
  shift1 = 2.*smetawdh + seta1wdh;    // The center of first eta strip
  //shift1 = (1.18-0.032/2-0.573/2)*2.54;   // From drawing of SMD
  shift2 = shift1 + (seta1wdh+seta12wdh)*2*74 + (seta1wdh+seta12wdh)
                  + (seta2wdh+seta12wdh); // The center of 76h eta strip

  for(i=0; i<mNEta; i++) {
    if(i<mNEta/2) mZlocal[i] = shift1 + (seta1wdh+seta12wdh)*2*i;
    else mZlocal[i] = shift2 + (seta2wdh+seta12wdh)*2*(i-75);
    mEta[i] = -log(tan(atan2(mRadius,mZlocal[i])/2.0));
  }

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub); mYlocal[0] = 0.0;

  mPhi.Set(mNSub); mPhi[0] =  0.0;

  //  cout<<" (BSMDE) shift1 "<<shift1<<endl;
  //cout<<" (BSMDE) shift2 "<<shift2<<endl;
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
  shift = mYWidth/2. - smdgaswdh - sphiwdh; // The position of center first phi strip

  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
  mZlocal.Set(mNEta);  mEta.Set(mNEta); 
  mYlocal.Set(mNSub);  mPhi.Set(mNSub); 
  
  // Eta variable ( Z direction)
  TArrayF etaw(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {etaw[i] = 0.1*i;} 
  etaw[mNEta]=mEtaMax;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (etaw[i+1] + etaw[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)

  mYlocal.Set(mNSub); mPhi.Set(mNSub);

  if(mMode.CompareTo("geant") == 0){ // For odd numbers ( default 15)
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
      mYlocal[i] = shift - (sphiwdh+sphidwdh)*2*i;
      if(mDetector==4) mYlocal[i] = - mYlocal[i];    // 26-oct-1999 => !!! ?? 
      mPhi[i] = atan2(mYlocal[i], mRadius);
    }
  }
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolIdBemc(const Int_t ivid, Int_t &module,Int_t &eta,
Int_t &sub, Int_t &detector)
{
  // Transition from Geant Volume Id to usual for BEMC and BPRS
  // See  emc/util/volid_bemc.F

  static Int_t emcIvid[5]={10000000,100000,100,10,1};
  Int_t emcChid[5], i, ividw, rl, phi, dep;

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
      printf("<W> StEmcGeom::getVolIdBemc => wrong value of dep %i \n",dep);
    }
    if     (rl==1) {
      phi+=((Int_t)toDeg(mPhiOffset[0])-75)/6;
      if     (phi<=0)  phi+=60;
      else if(phi>=61) phi-=60;
      module=phi;
    }
    else if(rl==2) {
      phi+=((Int_t)toDeg(mPhiOffset[1])-105)/6;
      if     (phi<=0)  phi+=60;
      else if(phi>=61) phi-=60;
      module=phi+60;
      sub   =(sub+1)%2+1;
    }
    else{
    printf("<E> getVolIdBemc -- error decoding BEMC Geant volume Id %i; rl=%i\n",
    ivid, rl);
    return 1;
    }
  }
  else {
    printf("<E> getVolIdBemc -- error decoding BEMC Geant volume Id %i=>%i\n",
    ivid, ividw);
    return 1;
  }
  //  printf(" vid %i m %3i eta %3i sub %3i dep %3i \n",
  // ivid,module,eta,sub,dep);  
  return 0;
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolIdBsmd(const Int_t ivid, Int_t &module,Int_t &eta,
Int_t &sub, Int_t &detector)
{
  // Transition from Geant Volume Id to usual for BSMDE and BSMDP
  // See  emc/util/volid_bsmd.F
  static Int_t smdIvid[5]={100000000,1000000,1000,100,1}; //matched with AGI&G2T
  Int_t smdChid[5], i, ividw, rl, phi, t, strip;

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
      phi+=((Int_t)toDeg(mPhiOffset[0])-75)/6;
      if     (phi<=0)  phi+=60;
      else if(phi>=61) phi-=60;
      module=phi;
    }
    else if(rl==2) {
      phi+=((Int_t)toDeg(mPhiOffset[1])-105)/6;
      if     (phi<=0)  phi+=60;
      else if(phi>=61) phi-=60;
      module=phi+60;
    }
    else{
      printf("<E> getVolIdBsmd -- error decoding BSMD Geant volume Id %i; rl=%i\n",
      ivid, rl);
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
      sub  = strip;
    }
    else {
      printf("<E> getVolIdBsmd: Type mismatch %i \n",t);
      return 1;
    }
  }
  else {
    printf("<E> getVolIdBsmd -- error decoding BSMD Geant volume Id %i=>%i\n",
    ivid, ividw);
    return 1;
  }
  //  printf(" vid %i m %3i eta %3i sub %3i type %3i \n",
  //ivid,module,eta,sub,type);  
  return 0;
}
// _____________________________________________________________________
Int_t StEmcGeom::getVolId(const Int_t ivid, Int_t &module,Int_t &eta,
Int_t &sub, Int_t &det)
{
  if     (mDetector==1||mDetector==2) return getVolIdBemc(ivid,module,eta,sub,det);
  else if(mDetector==3||mDetector==4) return getVolIdBsmd(ivid,module,eta,sub,det);
  else {
    printf("<E> getVolId -- wrong detectot number %i \n",mDetector);
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
  TArrayF etaw(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {etaw[i] = 0.05*i;} etaw[mNEta]=0.99;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (etaw[i+1] + etaw[i])/2.;
    mZlocal[i] = mRadius * sinh(mEta[i]);  // z=r/tan(theta) => 1./tan(theta) = sinh(eta)
  }

  // Phi variable ( Y direction)
  mYlocal.Set(mNSub);  
  mYlocal[0] =   mYWidth/2.;    mYlocal[1] = - mYlocal[0];  

  mPhi.Set(mNSub); 
  mPhi[0] =  atan2(mYWidth/2.,mRadius);    mPhi[1] = -mPhi[0];

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
    mEta[i] = -log(tan(atan2(mRadius,mZlocal[i])/2.0));
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
  TArrayF etaw(mNEta+1); Int_t i;

  for(i=0; i<mNEta; i++) {etaw[i] = 0.1*i;} 
  etaw[mNEta]=0.99;

  for(i=0; i< mNEta; i++){
    mEta[i]    = (etaw[i+1] + etaw[i])/2.;
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
void  StEmcGeom::printGeom()
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
  cout<<" mEtaMax    "<<mEtaMax<<endl;
  cout<<" mPhiOffset "<<mPhiOffset[0]<<"("<<toDeg(mPhiOffset[0])<<")   "
      <<mPhiOffset[1]<<"("<<toDeg(mPhiOffset[0])<<")"<<endl;
  cout<<" mPhiStep   "<<mPhiStep[0]<<"("<<toDeg(mPhiStep[0])<<")   "
      <<mPhiStep[1]<<"("<<toDeg(mPhiStep[1])<<")"<<endl;

  Int_t i;
  cout<<"\n Z grid and Eta grid "<<endl;
  for(i=0; i<mNEta; i++){
    cout<<" i "<<i<<" Zl "<<mZlocal[i]<<" Eta "<<mEta[i]<<endl; 
  }
  cout<<"\n Y grid and Phi grid "<<endl;
  for(i=0; i<mNSub; i++){
    cout<<" i "<<i<<" Yl "<<mYlocal[i]<<" Phi "<<mPhi[i]<<endl;
  }
  cout<<"\n Phi grid for center of module in STAR system "<<endl;
  for(i=0; i<mNModule; i++){
    if(i==60) cout<<"\n == "<<endl;
    printf(" %i phi %7.4f (%5.0f) \n",i+1,mPhiModule[i],mPhiModule[i]*C_DEG_PER_RAD);
  }
  cout<<"\n == "<<endl; 
}
// _____________________________________________________________________
void  StEmcGeom::compare(StEmcGeom &g, Bool_t key=kFALSE)
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
Float_t  StEmcGeom::relativeError(Float_t a, Float_t b)
{
  Float_t sum = fabs(a) + fabs(b), perr;
  if(sum == 0.0) return 0.0; // Both zero
  else {
    perr = 200.*fabs(a-b)/sum;
    return perr;
  }
}
// _____________________________________________________________________
void  StEmcGeom::printError(Float_t err)
{
  if(err>perr) printf(" | perr=%6.3f%% \n",err);
  else printf("\n");
}
