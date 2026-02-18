#ifndef StRHICfSimEvent_HH
#define StRHICfSimEvent_HH

#include "TObject.h"
#include "TString.h"

class StRHICfSimEvent : public TObject
{
    public: 
        StRHICfSimEvent();
        ~StRHICfSimEvent();

        void Clear(Option_t *option = "");

        // Event header
        void SetEventNumber(int idx);
        void SetProcessId(int id);
        void SetRHICfRunType(int type);
        void SetGeneratorIdx(int idx);

        void SetIsShowerTrigger();
        void SetIsType1Pi0Trigger();
        void SetIsHighEMTrigger();

        // Event generator information
        void SetXParton1(double val);
        void SetXParton2(double val);
        void SetXPdf1(double val);
        void SetXPdf2(double val);
        void SetQ2Fac(double val);
        void SetQ2Renorm(double val);
        void SetsHat(double val);
        void SettHat(double val);
        void SetuHat(double val);
        void SetPtHat(double val);
        void SetDiffractionAMass(double val);
        void SetDiffractionBMass(double val);
        void SetDiffractionCMass(double val);
        void SetDiffractionAEta(double val);
        void SetDiffractionBEta(double val);
        void SetDiffractionCEta(double val);

        void SetGenFinalParNum(int num);
        void SetGenFinalChargedParNum(int num);
        void SetPrimaryTrkNum(int num);

        Int_t GetEventNumber();
        Int_t GetProcessId();
        Int_t GetRHICfRunType();
        Int_t GetGeneratorIdx();

        Bool_t IsShowerTrigger();
        Bool_t IsType1Pi0Trigger();
        Bool_t IsHighEMTrigger();

        Double_t GetXParton1();
        Double_t GetXParton2();
        Double_t GetXPdf1();
        Double_t GetXPdf2();
        Double_t GetQ2Fac();
        Double_t GetQ2Renorm();
        Double_t GetsHat();
        Double_t GettHat();
        Double_t GetuHat();
        Double_t GetPtHat();
        Double_t GetDiffractionAMass();
        Double_t GetDiffractionBMass();
        Double_t GetDiffractionCMass();
        Double_t GetDiffractionAEta();
        Double_t GetDiffractionBEta();
        Double_t GetDiffractionCEta();
    
        Int_t GetGenFinalParNum();
        Int_t GetGenFinalChargedParNum();
        Int_t GetPrimaryTrkNum(); // MuMcTrack::IdVx() == 1 tracks number

    private:
        Int_t mEventNumber;
        Int_t mProcessId;
        Int_t mRHICfRunType;
        Bool_t mRHICfShowerTrig;
        Bool_t mRHICfType1Pi0Trig;
        Bool_t mRHICfHighEMTrig;
        Int_t mGeneratorIdx;

        Double_t mXParton1; // x of blue beam parton
        Double_t mXParton2; // x of yellow beam parton
        Double_t mXPdf1; // PDF times x for blue beam parton
        Double_t mXPdf2; // PDF times x for yellow beam parton
        Double_t mQ2Fac; // Factorization scale
        Double_t mQ2Ren; // Renormalization scale
        Double_t m_sHat; // Mandelstam s-variable
        Double_t m_tHat; // Mandelstam t-variable
        Double_t m_uHat; // Mandelstam u-variable
        Double_t mPtHat; // All of hadrons Pt for system
        Double_t mDiffAMass; // Mass of Diffraction A 
        Double_t mDiffBMass; // Mass of Diffraction B 
        Double_t mDiffCMass; // Mass of Diffraction C (Central) 
        Double_t mDiffAEta; // Eta of Diffraction A for chekcing the direction
        Double_t mDiffBEta; // Eta of Diffraction B for chekcing the direction
        Double_t mDiffCEta; // Eta of Diffraction C (Central) for chekcing the direction
        
        Int_t mGenFinalParNum;  // generator level final particle number
        Int_t mGenFinalChargedParNum;  // generator level final charged particle number
        Int_t mPrimaryTrkNum; // STAR simulation level primary trk number (NOT exactly same as nubmer of track has SimTrk::Primary() = true)

    ClassDef(StRHICfSimEvent,1)
};

#endif
