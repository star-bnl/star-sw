////////////////////////////////////////////////////////////
//                                                        //
//    StBarrelEmcCluster                                  //
//                                                        //
//    Pibero Djawotho                                     //
//    Indiana University                                  //
//    30 May 2007                                         //
//                                                        //
//    Cosmetic update by Michael Betancourt               //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StBarrelEmcCluster
#define STAR_StBarrelEmcCluster

class StGammaTower;

#include <iostream>
#include <vector>
#include "TVector3.h"

using namespace std;

class StBarrelEmcCluster: public TObject 
{

    public:
  
        StBarrelEmcCluster();
        ~StBarrelEmcCluster() {};
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StBarrelEmcCluster.h,v 1.6 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }

        float energy() const;
        const TVector3& position() const;
        const TVector3& momentum() const;
        StGammaTower *seed() const;
        StGammaTower *tower(int deta, int dphi) const;

        void setEnergy(float energy);
        void setPosition(const TVector3& position);
        void setMomentum(const TVector3& momentum);
        void setSeed(StGammaTower* tower);
        void setTower(int deta, int dphi, StGammaTower* tower);

    private:

        float mEnergy;
        TVector3 mPosition;
        TVector3 mMomentum;
        StGammaTower* mTowers[3][3];

  ClassDef(StBarrelEmcCluster, 1);
  
};

inline StBarrelEmcCluster::StBarrelEmcCluster() { memset(mTowers, 0, sizeof(mTowers)); }
inline float StBarrelEmcCluster::energy() const { return mEnergy; }
inline const TVector3& StBarrelEmcCluster::position() const { return mPosition; }
inline const TVector3& StBarrelEmcCluster::momentum() const { return mMomentum; }
inline StGammaTower* StBarrelEmcCluster::seed() const { return tower(0,0); }
inline StGammaTower* StBarrelEmcCluster::tower(int deta, int dphi) const { return mTowers[deta+1][dphi+1]; }

inline void StBarrelEmcCluster::setEnergy(float energy) { mEnergy = energy; }
inline void StBarrelEmcCluster::setPosition(const TVector3& position) { mPosition = position; }
inline void StBarrelEmcCluster::setMomentum(const TVector3& momentum) { mMomentum = momentum; }
inline void StBarrelEmcCluster::setSeed(StGammaTower* tower) { setTower(0, 0, tower); }
inline void StBarrelEmcCluster::setTower(int deta, int dphi, StGammaTower* tower) { mTowers[deta+1][dphi+1] = tower; }

ostream& operator<<(ostream& out, const StBarrelEmcCluster& cluster);

#endif
