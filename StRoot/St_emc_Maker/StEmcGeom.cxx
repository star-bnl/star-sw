/***************************************************************************
 *
 * $Id: StEmcGeom.cxx,v 1.4 2000/04/11 19:48:40 pavlinov Exp $
 *
 * Author: Aleksei Pavlinov , June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcGeom.cxx,v $
 * Revision 1.4  2000/04/11 19:48:40  pavlinov
 * Merge versions of Pavlinov and Ogawa
 *
 * Revision 1.3  2000/01/29 00:04:57  akio
 * temprary fix for endcap. need more work, but no more junk messages and crash
 *
 * Revision 1.2  1999/07/02 03:01:55  pavlinov
 * Little corrections for Linux
 *
 * Revision 1.1  1999/07/01 16:17:57  pavlinov
 * class StEmcGeom was created and maker was remade for new maker scheme
 *
 **************************************************************************/
#include <strings.h>
#include "StEmcGeom.h"
ClassImp(StEmcGeom)

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
  mDetector = det;
  // Common information for all detectors
  mNModule  = 120;
  mEtaMax   = 0.99;
  mPhiOffset.Set(2);  mPhiStep.Set(2); 

  mPhiOffset[0] = (75.-3.)/ 180. * C_PI;
  mPhiOffset[1] = (105.+3.)/180. * C_PI;
  mPhiStep[0] = -C_2PI /(mNModule/2); mPhiStep[1] = -mPhiStep[0];

  mPhiBound.Set(2);
  mPhiBound[0] = 75. /180. * C_PI;
  mPhiBound[1] = 105./180. * C_PI;
  mPhiStepHalf = 3. * C_PI/180.;

  mPhiModule.Set(mNModule);
  for(Int_t i=0; i<mNModule; i++){
    Int_t im = 2*i/mNModule;
    Float_t phiW=mPhiOffset[im]+mPhiStep[im]*i;
    while(phiW >=  C_PI) phiW -= C_2PI;
    while(phiW <  -C_PI) phiW += C_2PI;
    mPhiModule[i] = phiW;
  }

  switch (det){
  case 1:
  case 2:
    mRadius   = 225.405;   // Edge of SC1 (223.5+2* 0.9525)
    mYWidth   = 11.1716;  
    initBEMCorBPRS();
    break;
  case 3:
    mRadius   = 230.467;   // See find_pos_ems.F or Geant Geometry
    mYWidth   = 11.2014*2;  
    initBSMDE();
    break;
  case 4:
    mRadius   = 232.467;   // See find_pos_ems.F or Geant Geometry
    mYWidth   = 22.835;  
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
void StEmcGeom::initBEMCorBPRS() 
{
  mNEta     = 20;  
  mNSub     = 2;  
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
  mNEta     = 150;  
  mNSub     = 1;  
  mNes      = mNEta * mNSub;
  mNRaw     = mNes  * mNModule;
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
void StEmcGeom::initBSMDP()
{
  mNEta     = 10;  
  mNSub     = 15;  
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

  Float_t sphiwdh  = 0.668;           // half width for phi strips
  Float_t sphidwdh = 0.07874;         // half distance between strips in phi
  Float_t shift    = mYWidth/2. - 0.295 - sphiwdh; // The position of center first phi strip

  for(i=0; i<mNSub; i++){
    mYlocal[i] = shift - (sphiwdh+sphidwdh)*2*i;
    if(mDetector==4) mYlocal[i] = - mYlocal[i];    // 26-oct-1999 => !!! ?? 
    mPhi[i] = atan2(mYlocal[i], mRadius);
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
