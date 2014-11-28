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

#ifndef StHbtFsiLednicky_hh
#define StHbtFsiLednicky_hh

#include "StHbtMaker/Base/StHbtFsiWeight.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Base/StHbtThPair.hh"

class StHbtFsiLednicky : public  StHbtFsiWeight {
 public: 
// --- Constructor
  StHbtFsiLednicky(); // call SetDefaultCalcPar
// --- Destructor : nothing to explicitly delete
  ~StHbtFsiLednicky();

// --- Function to be called in the correlation function
  // see StThCorrFctn for details
  virtual double GetWeight(const StHbtThPair* aThPair);

// --- Setting

// >>> Calculation mode
  void SetDefaultCalcPar(); // Default is CoulOn, QuantumOn, StrongOn, 3BodyOff, Square, T0ApproxOff
  void SetCoulOn();
  void SetCoulOff();

  void SetQuantumOn();
  void SetQuantumOff();
  void SetStrongOn();
  void SetStrongOff();
  void Set3BodyOn();
  void Set3BodyOff();
  void SetSphere(); // use Spherical wave approximation
  void SetSquare(); // use use Square potential (only for p-p and pi+Pi-) otherwise, use spherical wave approx
  void SetT0ApproxOff();//only with  Spherical wave Approximation - this is default mode
  void SetT0ApproxOn(); 
 
// Test Lambda parameters
   void PrintLambdas();
   
   void SetNuclCharge(const double aNuclCharge); // for 3-body calculation
  void SetNuclMass(const double aNuclMass);

  virtual StHbtString Report();

protected:
  // Fsi weight output
  double mWei;
  double mWein;
  double mWeif;
  // Setting parameters
  int mItest;

  //int mNs;
  int mIch;
  int mIqs;
  int mIsi;
  int mI3c;
  double mNuclMass;
  double mNuclCharge;

  bool mSphereApp;
  bool mT0App;

  //Pair identification
  int mLL;
  short mNuclChargeSign;  
  bool mSwap;  // are particle in wright order ? 
  int const mLLMax;
  int* mNumProcessPair;
  int mNumbNonId;
  char** mLLName;

  // Interface to the fortran functions
  void FsiInit();
  void FsiSetLL();
  void FsiNucl();
  bool SetPid(const int aPid1,const int aPid2);

#ifdef __ROOT__
  ClassDef(StHbtFsiLednicky,1)
#endif
};

#endif
