#ifndef ZBosEvent_h
#define ZBosEvent_h

#include "TLorentzVector.h"
#include "TVector3.h"

#include "VecBosEvent.h"
#include "VecBosTrack.h"


/**
 * This class extends the data structure and functionality of #VecBosEvent which is saved in the
 * output ROOT tree. The events described by this class are assumed to pass the Z boson
 * requirements.
 */
class ZBosEvent : public VecBosEvent
{

public:

   ZBosEvent();
   ~ZBosEvent();

   TLorentzVector mP4ZBoson;
   TVector3           GetCandidate1_P3() const;
   TVector3           GetCandidate2_P3() const;
   TVector3           GetVecBosonP3() const;
   TLorentzVector     GetVecBosonP4() const;
   virtual void Process();
   virtual void ProcessMC();
   virtual void Clear(const Option_t* opt="");
   virtual void Print(const Option_t* opt="") const;
   bool         PassedCutZBos(float minElePt=sMinZEleCandPtLight) const;
   bool         PassedCutZMass(float minElePt=sMinZEleCandPtLight) const;

   //static const float sMinElectronPtLight; //!
   //static const float sMinElectronPtHard;  //!
   //static const float sMinNeutrinoPt;      //!
   static const float sMinZEleCandPtLight; //! S.Fazio 30Sep2013
   static const float sMinZEleCandPtHard;  //!
   static const float sMinZMass;           //! 
   static const float sMaxZMass;           //! 

protected:

   void      CalcZBosP4();

   float            mZBosMass;   
   TVector3         mCand1P3; 
   TVector3         mCand2P3; 
   TLorentzVector   mP4Cand1; 
   TLorentzVector   mP4Cand2;

   ClassDef(ZBosEvent, 1);
};

#endif
