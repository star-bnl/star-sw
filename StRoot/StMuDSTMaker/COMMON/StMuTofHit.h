#ifndef StMuTofHit_hh
#define StMuTofHit_hh

#include <sstream>
#include "StObject.h"
#include "StThreeVectorF.hh"
//#include "StarClassLibrary/StParticleDefinition.hh"

//class StTrack;

class StMuTofHit : public StObject {
public:
    StMuTofHit();
    
    //StMuTofHit(const StMuTofHit&);
    //StMuTofHit& operator=(const StMuTofHit&);
    ~StMuTofHit();

    int Iconf() const;
    int trayIndex() const;
    int moduleIndex() const;
    int cellIndex() const;
    int daqIndex() const;
    int adc() const;
    float timeOfFlight() const;
    float pathLength() const;
    float beta() const;

    int associatedTrackId() const;
    StThreeVectorF projectedPoint() const;
    //    StTrack* associatedTrack();
    //    const StTrack* associatedTrack() const;
    float tofExpectedAsElectron() const;
    float tofExpectedAsPion() const;
    float tofExpectedAsKaon() const;
    float tofExpectedAsProton() const;

    float sigmaElectron() const;
    float sigmaPion() const;
    float sigmaKaon() const;
    float sigmaProton() const;

    int particleHypothesis() const;
    //    StParticleDefinition* particleHypothesis();
    //    const StParticleDefinition* particleHypothesis() const;


    void setIconf(int);
    void setTrayIndex(int);
    void setModuleIndex(int);
    void setCellIndex(int);
    void setDaqIndex(int);
    void setADC(int);
    void setTimeOfFlight(float);
    void setPathLength(float);
    void setBeta(float);
    //    void setAssociatedTrack(StTrack*);
    void setAssociatedTrackId(int);
    void setProjectedPoint(const StThreeVectorF&);
    void settofExpectedAsElectron(float);
    void settofExpectedAsPion(float);
    void settofExpectedAsKaon(float);
    void settofExpectedAsProton(float);
    void setsigmaElectron(float);
    void setsigmaPion(float);
    void setsigmaKaon(float);
    void setsigmaProton(float);

    void setparticleHypothesis(int);
    //    void setparticleHypothesis(StParticleDefinition*);

 protected:
    Int_t mIconf;
    Int_t mTrayIndex;
    Int_t mModuleIndex;
    Int_t mCellIndex;
    Int_t mDaqIndex;
    Int_t mADC;
    Float_t mTimeOfFlight;
    Float_t mPathLength;
    Float_t mBeta;
    //   StTrack* mAssociatedTrack;
    Int_t mAssociatedTrackId;
    StThreeVectorF mProjectedPoint;
    Float_t mTOFExpectedAsElectron;
    Float_t mTOFExpectedAsPion;
    Float_t mTOFExpectedAsKaon;
    Float_t mTOFExpectedAsProton;
    Float_t mSigmaElectron;
    Float_t mSigmaPion;
    Float_t mSigmaKaon;
    Float_t mSigmaProton;
    //    StParticleDefinition* mParticleHypothesis;
    Int_t mParticleHypothesis;

    ClassDef(StMuTofHit,1)
};

#endif
