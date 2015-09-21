/**************************************************************************
 *
 * $Id: StFmsPointPair.h,v 2.1 2015/09/14 16:15:50 ullrich Exp $
 *
 * Author: Akio Ogawa, Sep 2015
 **************************************************************************
 *
 * Description: Declaration of StFmsPointPair, the StEvent FMS pi0/EMJet
 *
 **************************************************************************
 *
 * $Log: StFmsPointPair.h,v $
 * Revision 2.1  2015/09/14 16:15:50  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFmsPointPair_h
#define StFmsPointPair_h

#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
#include "StObject.h"
#include "StEnumerations.h"
#include "StFmsPoint.h"

class StFmsPointPair : public StObject {
public:
    StFmsPointPair();
    StFmsPointPair(StFmsPoint* p);
    StFmsPointPair(StFmsPoint* p1, StFmsPoint* p2);
    ~StFmsPointPair();
        
    const int nPoints() const;
    void addPoint(StFmsPoint* p);
    vector<StFmsPoint*>& points();
    StFmsPoint* point(int v);
  
    const StLorentzVectorF& fourMomentum() const;
    float energy() const;
    float pT() const;
    float eta() const;
    float phi() const;
    float mass() const;
    float dgg() const;            //only make sense if nPoint=2
    float zgg() const;            //only make sense if nPoint=2
    unsigned int fpsPid() const;  //each digit {0=bad,1=gamma,2=hadron,3=electton}
                                  //for pair(npoint=2), 11=gg, 22=hh, 33=ee, 13=ge,etc
                                  //for nPoint>2, LSD is first point and MSD is last 

    enum StFmsPointPairConsts {kFmsPointMaxCone=3}; //!
    float coneRadius(int cone) const;
    float coneEnergy(int cone) const;
    float coneEnergyFraction(int cone) const;
    void setConeEnergy(int cone, float energy);

    void print(int option=0);

private:
    vector<StFmsPoint*> mPoints;    //!
    StLorentzVectorF mFourMomentum; //!  
    UInt_t  mFpsPid;                //! 
    Float_t mConeRadius[kFmsPointMaxCone]; //! cone radius
    Float_t mConeEnergy[kFmsPointMaxCone]; //! sum of fms hit(cell) energy within a cone

    ClassDef(StFmsPointPair, 1)
};

inline const int StFmsPointPair::nPoints() const {return mPoints.size();}
inline vector<StFmsPoint*>& StFmsPointPair::points() {return mPoints;}
inline const StLorentzVectorF& StFmsPointPair::fourMomentum() const { return mFourMomentum; }
inline float StFmsPointPair::energy() const { return mFourMomentum.e(); }
inline float StFmsPointPair::pT() const { return mFourMomentum.perp(); }
inline float StFmsPointPair::eta() const { return mFourMomentum.pseudoRapidity(); }
inline float StFmsPointPair::phi() const { return mFourMomentum.phi(); }
inline float StFmsPointPair::mass() const { return mFourMomentum.m(); }
inline unsigned int  StFmsPointPair::fpsPid() const { return mFpsPid; }
 
#endif  // StFmsPointPair_h

