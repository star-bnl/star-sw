#ifndef StRHICfSimRHICfPoint_HH
#define StRHICfSimRHICfPoint_HH

#include <vector>
#include "TObject.h"
#include "StRHICfSimPar.h"

using namespace std;

class StRHICfSimRHICfPoint : public TObject
{
    public: 
        StRHICfSimRHICfPoint();
        ~StRHICfSimRHICfPoint();

        void Clear(Option_t *option = "");

        void SetTowerIdx(int val);
        void SetPID(int pid);
        void SetPointPos(float x, float y);
        void SetPointEnergy(float pid1, float pid2);

        Int_t GetTowerIdx();
        Int_t GetPID();
        Float_t GetPointPos(int xy);
        Float_t GetPointEnergy(int particle);

    private:

        Int_t mTowerIdx;
        Int_t mParticleID;

        Float_t mPointPos[rXYNum];
        Float_t mPointEnergy[2];

    ClassDef(StRHICfSimRHICfPoint,1)
};

#endif
