/**************************************************************************
 *
 * $Id: StFmsPointPair.h,v 2.4 2017/02/20 16:32:58 ullrich Exp $
 *
 * Author: Akio Ogawa, Sep 2015
 **************************************************************************
 *
 * Description: Declaration of StFmsPointPair, the StEvent FMS pi0/EMJet
 *
 **************************************************************************
 *
 * $Log: StFmsPointPair.h,v $
 * Revision 2.4  2017/02/20 16:32:58  ullrich
 * Changing F to D for StLorentzVector
 *
 * Revision 2.3  2016/06/07 15:51:34  akio
 * Making code better based on Coverity reports
 *
 * Revision 2.2  2015/10/21 14:52:54  ullrich
 * Added methods x() and y()
 *
 * Revision 2.1  2015/09/14 16:15:50  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFmsPointPair_h
#define StFmsPointPair_h

#include "StLorentzVectorD.hh"
#include "StThreeVectorD.hh"
#include "StObject.h"
#include "StEnumerations.h"
#include "StFmsPoint.h"

class StFmsPointPair : public StObject {
public:
    StFmsPointPair();
    StFmsPointPair(StFmsPoint* p);
    StFmsPointPair(StFmsPoint* p1, StFmsPoint* p2);
    ~StFmsPointPair();
        
    int nPoints() const;
    void addPoint(StFmsPoint* p);
    vector<StFmsPoint*>& points();
    StFmsPoint* point(int v);
  
    const StLorentzVectorD& fourMomentum() const;
    float energy() const;
    float pT() const;
    float eta() const;
    float phi() const;
    float mass() const;
    float dgg() const;            //only make sense if nPoint=2
    float zgg() const;            //only make sense if nPoint=2
    float x() const;  
    float y() const;;
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
    StLorentzVectorD mFourMomentum; //!  
    UInt_t  mFpsPid;                //! 
    Float_t mConeRadius[kFmsPointMaxCone]; //! cone radius
    Float_t mConeEnergy[kFmsPointMaxCone]; //! sum of fms hit(cell) energy within a cone

    ClassDef(StFmsPointPair, 1)
};

inline int StFmsPointPair::nPoints() const {return mPoints.size();}
inline vector<StFmsPoint*>& StFmsPointPair::points() {return mPoints;}
inline const StLorentzVectorD& StFmsPointPair::fourMomentum() const { return mFourMomentum; }
inline float StFmsPointPair::energy() const { return (float)mFourMomentum.e(); }
inline float StFmsPointPair::pT() const { return (float)mFourMomentum.perp(); }
inline float StFmsPointPair::eta() const { return (float)mFourMomentum.pseudoRapidity(); }
inline float StFmsPointPair::phi() const { return (float)mFourMomentum.phi(); }
inline float StFmsPointPair::mass() const { return (float)mFourMomentum.m(); }
inline unsigned int  StFmsPointPair::fpsPid() const { return mFpsPid; }
 
#endif  // StFmsPointPair_h

