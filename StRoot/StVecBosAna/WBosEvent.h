#ifndef WBosEvent_h
#define WBosEvent_h

#include "TVector3.h"
#include "TLorentzVector.h"

#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TH2.h"

#include "VecBosEvent.h"
#include "VecBosTrack.h"

#include <sstream>

/**
 * This class extends the data structure and functionality of #VecBosEvent which is saved in the
 * output ROOT tree. The events described by this class are assumed to pass the W boson
 * requirements.
 */
class WBosEvent : public VecBosEvent
{
public:

  WBosEvent(float minTrackPt=0, int RhicRunId=11, bool otherSolution=false);

   VecBosTrack& GetElectronTrack() const;
   TVector3     GetElectronP3() const;
   TVector3     GetNeutrinoP3() const;
   TVector3     GetNeutrinoP3Other() const;
   TVector3     CalcMissingEnergyP3() const;
   double       CalcSignedPtBalance() const;
   TVector3     GetVecBosonP3() const;
   TVector3     GetVecBosonP3FirstSolution() const;
   TVector3     GetVecBosonP3OtherSolution() const;
   TLorentzVector GetVecBosonP4() const;
   TLorentzVector GetVecBosonP4OtherSolution() const;
   void         PredictionAnEvol(int McType);
   void         PredictionAn(int McType);
   virtual void Process();
   virtual void ProcessPersistent();
   virtual void ProcessMC(int McType);
   virtual void Clear(const Option_t* opt="");
   virtual void Print(const Option_t* opt="") const;
   bool         PassedCutWBos(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosNoEndcap(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosPlus(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosPlusNoEndcap(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosPlusPl(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosMinus(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosMinusNoEndcap(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWBosMinusPl(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWPlusAn(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutWMinusAn(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutQcdBkg(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutQcdBkgPlus(float minElePt=sMinElectronPtLight) const;
   bool         PassedCutQcdBkgMinus(float minElePt=sMinElectronPtLight) const;
   //Double_t     An_evol_ZK;

   static bool sUseOtherSolution;          //!< If true calculates the W kinematics using the second choice solution for the z-component
   static const float sMinElectronPtLight; //!
   static const float sMinElectronPtHard;  //!
   static const float sMinNeutrinoPt;      //!
   static const float sMinRecoilPt;        //!
   static const float sMaxRecoilPt;        //!

protected:

   void      ReconstructNeutrinoZ();

   float     mWBosMass;   //
   TVector3  mElectronP3; //
   TVector3  mNeutrinoP3;      // Neutrion momentum based on the default (correct/most likely) p_z solution
   TVector3  mNeutrinoP3Other; // Neutrino momentum based on the other ("wrong") p_z solution

   ClassDef(WBosEvent, 1);
};

#endif
