/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Calculate correlation weight using R.Lednicky's code 
 *  Use the fortran files : FsiLednickyWeight.F and FsiTools.F
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "StHbtMaker/ThCorrFctn/StHbtFsiLednicky.h"
#include "StarCallf77.h"
#include <Stsstream.h>


#ifdef SOLARIS
# ifndef false
typedef int bool;
#define false 0
#define true 1
# endif
#endif

// --- Prototype of the function used in the weight calculator 
//     (in FsiWeightLedinicky.F)
#define fsiin F77_NAME(fsiin,FSIIN)
extern "C" {void type_of_call F77_NAME(fsiin,FSIIN)(const int &itest,const int &ich, const int &iqs, const int &isi,const int &i3c);}
#define llini F77_NAME(llini,LLINI)
extern "C" {void type_of_call F77_NAME(llini,LLINI)(const int &lll,const int &ns, const int &itest);}

#define fsinucl F77_NAME(fsinucl,FSINUCL)
extern "C" {void type_of_call  F77_NAME(fsinucl,FSINUCL)(const double &mn,const double &cn);}
#define fsimomentum F77_NAME(fsimomentum,FSIMOMENTUM)
extern "C" {void type_of_call F77_NAME(fsimomentum,FSIMOMENTUM)(double &p1,double &p2);}
#define fsiposition F77_NAME(fsiposition,FSIPOSITION)
extern "C" {void type_of_call F77_NAME(fsiposition,FSIPOSITION)(double &x1,double &x2);}
#define fsiw F77_NAME(fsiw,FSIW)
extern "C" {void type_of_call F77_NAME(fsiw,FSIW)(const int &i,double &weif,
						  double &wei,double &wein);}
#define ltran12 F77_NAME(ltran12,LTRAN12)
extern "C" {void type_of_call ltran12_();}

// Test function for Lambda potential
#define printlam F77_NAME(printlam,PRINTLAM)
extern "C" {void type_of_call printlam_();}

// --- Additional prototyping of some CERN functions (in FsiTool.F)
typedef float   REAL;
typedef struct { REAL re; REAL im; } COMPLEX;
#define cgamma F77_NAME(cgamma,CGAMMA)
extern "C" {COMPLEX type_of_call cgamma_(COMPLEX*);}

#ifdef __ROOT__
ClassImp(StHbtFsiLednicky)
#endif

StHbtFsiLednicky::StHbtFsiLednicky() : StHbtFsiWeight(),
  mItest(0),mIch(1),mIqs(1),mIsi(1),mI3c(0),
  mNuclMass(1.),mNuclCharge(0.),
  mSphereApp(false),mT0App(false),mNuclChargeSign(1), mLLMax(30),mNumbNonId(0) {

  mLLName=new char*[mLLMax+1];
  mNumProcessPair=new int[mLLMax+1];
  int i;
  for (i=1;i<=mLLMax;i++) {mLLName[i]=new char[40];mNumProcessPair[i]=0;}
  strcpy( mLLName[1],"neutron neutron");
  strcpy( mLLName[2],"proton proton");
  strcpy( mLLName[3],"neutron proton");
  strcpy( mLLName[4],"alpha alpha");
  strcpy( mLLName[5],"pi+ pi-");
  strcpy( mLLName[6],"pi0 pi0");
  strcpy( mLLName[7],"pi+ pi+");
  strcpy( mLLName[8],"neutron deuteron");
  strcpy( mLLName[9],"proton deuteron");
  strcpy( mLLName[10],"pi+ K-");
  strcpy( mLLName[11],"pi+ K+");
  strcpy( mLLName[12],"pi+ proton");
  strcpy( mLLName[13],"pi- proton");
  strcpy( mLLName[14],"K+ K-");
  strcpy( mLLName[15],"K+ K+");
  strcpy( mLLName[16],"K+ proton");
  strcpy( mLLName[17],"K- proton");
  strcpy( mLLName[18],"deuteron deuteron");
  strcpy( mLLName[19],"deuton alpha");
  strcpy( mLLName[20],"triton triton");
  strcpy( mLLName[21],"triton alpha");
  strcpy( mLLName[22],"K0 K0");
  strcpy( mLLName[23],"K0 K0b");
  strcpy( mLLName[24],"deuteron triton");
  strcpy( mLLName[25],"proton triton");
  strcpy( mLLName[26],"proton alpha");
  strcpy( mLLName[27],"proton lambda");
  strcpy( mLLName[28],"neutron lambda");
  strcpy( mLLName[29],"Lambda lambda");// gael 21May02
  strcpy( mLLName[30],"Proton Anti-proton");// gael 21May02
  FsiInit();
  FsiNucl();
};


double StHbtFsiLednicky::GetWeight(const StHbtThPair* aThPair){

  if (!SetPid(aThPair->GetPid1(),aThPair->GetPid2())) {
    mWeightDen=1.;
    return 1;    
  } else { // Good Pid
    const StHbtLorentzVector*  p;
    p=(aThPair->GetRealMomentum1());
    double p1[]={p->px(),p->py(),p->pz()};
    p=(aThPair->GetRealMomentum2());
    double p2[]={p->px(),p->py(),p->pz()};
    if ((p1[0]==p2[0])&&(p1[1]==p2[1])&&(p1[2]==p2[2])) {
      mWeightDen=0.;
      return 0;  
    } 
    if (mSwap) {
      fsimomentum(*p2,*p1);
    } else {
      fsimomentum(*p1,*p2);
    }
    p=(aThPair->GetEmPoint1());
//     cout << "Pid1:dans GetWeight = " << aThPair->GetPid1() << endl;
//     cout << "Pid2:dans GetWeight = " << aThPair->GetPid2() << endl;
//     cout << "LL:in GetWeight = " << mLL << endl;
    double x1[]={p->x(),p->y(),p->z(),p->t()};
    p=(aThPair->GetEmPoint2());
    double x2[]={p->x(),p->y(),p->z(),p->t()};
    if ((x1[0]==x2[0])&&(x1[1]==x2[1])&&(x1[2]==x2[2])&&(x1[3]==x2[3])) {
      mWeightDen=0.;
      return 0;  
    } 
    if (mSwap) {
      fsiposition(*x1,*x2);
    } else {
      fsiposition(*x2,*x1);
    }
    FsiSetLL();
    ltran12();
    fsiw(1,mWeif,mWei,mWein);
    if (mI3c==0) return mWein;
    mWeightDen=mWeif;
    return mWei;
  };
};

StHbtString StHbtFsiLednicky::Report() {
  ostrstream tStr; 
  tStr << "Lednicky afterburner calculation for  Correlation -  Report" << endl;
  tStr << "    Setting : Quantum : " << ((mIqs) ? "On" : "Off"); 
  tStr << " - Coulbomb : " << ((mIch) ? "On" : "Off") ;
  tStr << " - Strong : " << ((mIsi) ? "On" : "Off");
  tStr << endl;
  tStr << "              3-Body : " << ((mI3c) ? "On"  : "Off") ;
  if (mI3c) tStr << " Mass=" <<  mNuclMass << " - Charge= " << mNuclCharge ;
  tStr << endl;
  tStr << "    " << mNumProcessPair[0] << " Pairs have been Processed :" << endl;
  int i;
  for(i=1;i<=mLLMax;i++) { 
    if (mNumProcessPair[i])
      tStr << "         "<<  setw(8) << mNumProcessPair[i] << " " << mLLName[i] << endl;
  }
  if (mNumbNonId)
    tStr << "         "<< setw(8) << mNumbNonId << " Non Identified" << endl;
  StHbtString returnThis = tStr.str();
  return returnThis;
}

void StHbtFsiLednicky::FsiInit(){
  cout << "*******************StHbtFsiLednicky check FsiInit ************" << endl;
  cout <<"mItest dans FsiInit() = " << mItest << endl;
  cout <<"mIch dans FsiInit() = " << mIch << endl;
  cout <<"mIqs dans FsiInit() = " << mIqs << endl;
  cout <<"mIsi dans FsiInit() = " << mIsi << endl;
  cout <<"mI3c dans FsiInit() = " << mI3c << endl;
  fsiin(mItest,mIch,mIqs,mIsi,mI3c);
};

void StHbtFsiLednicky::FsiNucl(){
  cout << "*******************StHbtFsiLednicky check FsiNucl ************" << endl;
  cout <<"mNuclMass dans FsiNucl() = " << mNuclMass << endl;
  cout <<"mNuclCharge dans FsiNucl() = " << mNuclCharge << endl;
  cout <<"mNuclChargeSign dans FsiNucl() = " << mNuclChargeSign << endl;
  fsinucl(mNuclMass,mNuclCharge*mNuclChargeSign);
};

void StHbtFsiLednicky::FsiSetLL(){
  int tNS;
  if (mSphereApp||(mLL>5)) {
    if (mT0App) { tNS=4;} 
    else {tNS=2;};
  } else { tNS=1;};
//   cout <<"mLL dans FsiSetLL() = "<< mLL << endl;
//   cout <<"tNS dans FsiSetLL() = "<< tNS << endl;
//   cout <<"mItest dans FsiSetLL() = "<< mItest << endl;
  llini(mLL,tNS,mItest);
}
         
bool StHbtFsiLednicky::SetPid(const int aPid1,const int aPid2) {

  static const int sPi0Pid=111;
  static const int sPionPid=211; 
  static const int sK0Pid=311;
  static const int sKPid=321;
  static const int sNeutPid=2112;
  static const int sProtPid=2212;
  static const int sLamPid=3122;
  //  static const int sLamLamPid=3122;

  //  cout << "Setting PID to " << aPid1 << " " << aPid2 << endl;

  int tPidl,tPidh;
  int tChargeFactor=1;
  
  if (abs(aPid1)<abs(aPid2)) {
    if (aPid1<0) tChargeFactor=-1;
    tPidl=aPid1*tChargeFactor;
    tPidh=aPid2*tChargeFactor;
    mSwap=false;
  } else {
    if (aPid2<0) tChargeFactor=-1;
    tPidl=aPid2*tChargeFactor;
    tPidh=aPid1*tChargeFactor;
    mSwap=true;
  }
  switch (tPidl) {
  case sPionPid:
    switch (tPidh) {
    case -sPionPid:   mLL=5; tChargeFactor*=1 ;break;
    case sPionPid:    mLL=7; tChargeFactor*=1 ;break;
    case -sKPid:      mLL=10;tChargeFactor*=1 ;break;  
    case sKPid:       mLL=11;tChargeFactor*=1 ;break;  
    case sProtPid:    mLL=12;tChargeFactor*=1 ;break;
    case -sProtPid:   mLL=13;tChargeFactor*=-1;break;
    default: mLL=0;
    }
    break;
  case sProtPid:
    switch (tPidh) {
    case sProtPid:    mLL=2; tChargeFactor*=1 ;break;
    case sLamPid:     mLL=27;tChargeFactor*=1 ;break;
    case -sProtPid:   mLL=30;tChargeFactor*=1 ;break;
    default: mLL=0;
    };
    break;
  case sKPid:
    switch (tPidh) {
    case -sKPid:      mLL=14;tChargeFactor*=1 ;break;
    case sKPid:       mLL=15;tChargeFactor*=1 ;break;
    case sProtPid:    mLL=16;tChargeFactor*=1 ;break;
    case -sProtPid:   mLL=17;tChargeFactor*=-1 ;break;
    default: mLL=0;
    };
    break;    
  case sK0Pid:
    switch (tPidh) {
    case sK0Pid:         mLL=22;tChargeFactor*=1 ;break;
    case -sK0Pid:        mLL=23;tChargeFactor*=1 ;break;
    default: mLL=0;
    };
    break;   
  case sPi0Pid:
    switch (tPidh) {
    case sPi0Pid:        mLL=6; tChargeFactor*=1 ;break;
    default: mLL=0;
    };
    break;
  case sNeutPid:
    switch (tPidh) {
    case sNeutPid:      mLL=1; tChargeFactor*=1 ;break;
    case sProtPid:      mLL=3; tChargeFactor*=1 ;break;
    case sLamPid:       mLL=28;tChargeFactor*=1 ;break;
    default: mLL=0;
    };
    break;                                             //Gael 21May02 
  case sLamPid:                                        //Gael 21May02 
    switch (tPidh) {                                   //Gael 21May02 
    case sLamPid:       mLL=29;tChargeFactor*=1 ;break;//Gael 21May02  
    default: mLL=0;                                    //Gael 21May02 
    };                                                 //Gael 21May02 
    break;                                             //Gael 21May02 
  default: mLL=0;
  }
  if (tChargeFactor!=mNuclChargeSign) {
    mNuclChargeSign=tChargeFactor;
    FsiNucl();
  }
  (mNumProcessPair[0])++;
  if (mLL) {
    (mNumProcessPair[mLL])++;
    return true;
  } else {
    mNumbNonId++;
    return false;
  }
  cout << "*******************StHbtFsiLednicky check SetPid ************" << endl;
  cout << "mLL=="<< mLL << endl;
  cout << "mNuclCharge=="<< mNuclCharge << endl;

}    
StHbtFsiLednicky::~StHbtFsiLednicky() 
{ /* no-op */ };


inline  void StHbtFsiLednicky::SetNuclCharge(const double aNuclCharge) {mNuclCharge=aNuclCharge;FsiNucl();};
inline  void StHbtFsiLednicky::SetNuclMass(const double aNuclMass){mNuclMass=aNuclMass;FsiNucl();};

inline  void StHbtFsiLednicky::SetSphere(){mSphereApp=true;};
inline  void StHbtFsiLednicky::SetSquare(){mSphereApp=false;};
inline  void StHbtFsiLednicky::SetT0ApproxOn(){ mT0App=true;};
inline  void StHbtFsiLednicky::SetT0ApproxOff(){ mT0App=false;};
inline  void StHbtFsiLednicky::SetDefaultCalcPar(){
  mItest=1;mIqs=1;mIsi=1;mI3c=0;mIch=1;FsiInit();
  mSphereApp=false;mT0App=false;};

inline  void StHbtFsiLednicky::SetCoulOn(){mItest=1;mIch=1;FsiInit();};
inline  void StHbtFsiLednicky::SetCoulOff(){mItest=1;mIch=0;FsiInit();};
inline  void StHbtFsiLednicky::SetQuantumOn(){mItest=1;mIqs=1;FsiInit();};
inline  void StHbtFsiLednicky::SetQuantumOff(){mItest=1;mIqs=0;FsiInit();};
inline  void StHbtFsiLednicky::SetStrongOn(){mItest=1;mIsi=1;FsiInit();};
inline  void StHbtFsiLednicky::SetStrongOff(){mItest=1;mIsi=0;FsiInit();};
inline  void StHbtFsiLednicky::Set3BodyOn(){mItest=1;mI3c=1;FsiInit();FsiNucl();};
inline  void StHbtFsiLednicky::Set3BodyOff(){mItest=1;mI3c=0;FsiInit();mWeightDen=1.;FsiNucl();};

void StHbtFsiLednicky::PrintLambdas() {printlam();};

  

