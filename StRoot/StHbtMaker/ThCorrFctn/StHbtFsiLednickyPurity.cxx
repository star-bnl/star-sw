/***************************************************************************
 *
 * $Id: 
 *
 * Author: Adam Kisiel, Warsaw University of Technology, Poland
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

#include "StHbtMaker/ThCorrFctn/StHbtFsiLednickyPurity.h"
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

// --- Additional prototyping of some CERN functions (in FsiTool.F)
typedef float   REAL;
typedef struct { REAL re; REAL im; } COMPLEX;
#define cgamma F77_NAME(cgamma,CGAMMA)
extern "C" {COMPLEX type_of_call cgamma_(COMPLEX*);}

#ifdef __ROOT__
ClassImp(StHbtFsiLednickyPurity)
#endif

StHbtFsiLednickyPurity::StHbtFsiLednickyPurity() : StHbtFsiLednicky(),
  mPurity1(1.0), mPurity2(1.0) 
{
  /* no-op */
};


double StHbtFsiLednickyPurity::GetWeight(const StHbtThPair* aThPair){

  if (!SetPid(aThPair->GetPid1(),aThPair->GetPid2())) {
    mWeightDen=1.;
    return 1;    
  } else { // Good Pid
    // Check the purity
    if (aThPair->GetPid1() == aThPair->GetPid2()) {
      if (mRandom.Rndm() > mPurity1) {
	mWeightDen=1.;
	return 1;    
      }
    }
    else if ((mRandom.Rndm() > mPurity1) || (mRandom.Rndm() > mPurity1)) {
      mWeightDen=1.;
      return 1;    
    }
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

StHbtString StHbtFsiLednickyPurity::Report() {
  ostrstream tStr; 
  tStr << "Lednicky afterburner calculation for  Correlation -  Report" << endl;
  tStr << "    Setting : Quantum : " << ((mIqs) ? "On" : "Off"); 
  tStr << " - Coulbomb : " << ((mIch) ? "On" : "Off") ;
  tStr << " - Strong : " << ((mIsi) ? "On" : "Off");
  tStr << endl;
  tStr << "              3-Body : " << ((mI3c) ? "On"  : "Off") ;
  if (mI3c) tStr << " Mass=" <<  mNuclMass << " - Charge= " << mNuclCharge ;
  tStr << endl;
  tStr << "First particle purity : " << mPurity1 << " second : " << mPurity2 << endl;
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

StHbtFsiLednickyPurity::~StHbtFsiLednickyPurity() 
{ /* no-op */ };


inline  void StHbtFsiLednickyPurity::SetPurity(const double aPurity) { mPurity1=aPurity; mPurity2 = aPurity; }
inline  void StHbtFsiLednickyPurity::SetPurity(const double aPurity1, const double aPurity2) { mPurity1 = aPurity1; mPurity2 = aPurity2; }


  

