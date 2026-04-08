#ifndef StRHICfSimTrack_HH
#define StRHICfSimTrack_HH

#include <algorithm>
#include <vector>
#include "TObject.h"
#include "TVector3.h"
#include "TMath.h"

using namespace std;

class StRHICfSimTrack : public TObject
{
    public: 
        StRHICfSimTrack();
        ~StRHICfSimTrack();

        void Clear(Option_t *option = "");

        void SetIsPrimary();
        void SetIsSimPropagate();
        void SetIsFinal();
        void SetIsRHICfHit();
        void SetId(int id);
        void SetPid(int pid);
        void SetParentId(int id);
        void SetDaughterNum(int num);
        void SetEnergy(float energy);
        void SetMomentum(float px, float py, float pz);
        void SetVertexStart(float x, float y, float z);
        void SetVertexEnd(float x, float y, float z);

        Bool_t IsPrimary();
        Bool_t IsSimPropagate();
        Bool_t IsFinal();
        Bool_t IsRHICfHit();
        Int_t GetId();
        Int_t GetPid();
        Int_t GetParentId();
        Int_t GetDaughterNum();
        Double_t GetE();
        Double_t GetPx();
        Double_t GetPy();
        Double_t GetPz();
        Double_t GetPt();
        Double_t GetEta();
        Double_t GetVxStart();
        Double_t GetVyStart();
        Double_t GetVzStart();
        Double_t GetVxEnd();
        Double_t GetVyEnd();
        Double_t GetVzEnd();

    private:
        Bool_t mIsPrimary; // it is primary track
        Bool_t mIsSimPropagate; // is track propagated to RHICf simulation
        Bool_t mIsFinal; // is track final state
        Bool_t mIsRHICfHit; // is particle hitted RHICf acceptance
        Int_t mId; // StMuMcTrack id()
        Int_t mPid; // PDG encoding 
        Int_t mParentTrkId; // parent track id 
        Int_t mDaughterNum;  // doughter Num (1 = transport, 2 = decay or scattering, 2 < shower in meterial)
        Double_t mEnergy; // StMuMcTrack E() [GeV]
        Double_t mMom[3]; // StMuMcTrack Pxyz() [GeV/c]
        Double_t mVertexStart[3]; // StMuMcVertex(StMuMcTrack.IdVx()) XyzV() [cm]
        Double_t mVertexEnd[3]; // StMuMcVertex(StMuMcTrack.IdVx()) XyzV() [cm]


    ClassDef(StRHICfSimTrack,1)
};

#endif
