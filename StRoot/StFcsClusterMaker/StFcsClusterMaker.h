// $Id: StFcsClusterMaker.h,v 1.9 2019/11/22 17:26:17 akio Exp $
// $Log: StFcsClusterMaker.h,v $
// Revision 1.9  2019/11/22 17:26:17  akio
// fix typo
//
// Revision 1.8  2019/11/22 15:51:16  akio
// adding categorization
//
// Revision 1.7  2019/07/03 16:13:29  akio
// separate neighbor distance for ecal and hcal
//
// Revision 1.6  2019/06/27 16:12:42  akio
// Using getLocalXYinCell for cell/cluster distance measure and in moment analysis
//
// Revision 1.5  2019/06/26 18:00:34  akio
// added some adjustable parameters and way to select how to get energy
//
// Revision 1.4  2019/06/25 16:38:17  akio
// Added TOWER_E_RATIO2SPLIT, neighbor clusters
//
// Revision 1.3  2019/06/21 16:32:42  akio
// Added neighbor cluster and factor threshold for cluster splitting at valley
//
// Revision 1.2  2019/06/07 18:17:27  akio
// added summing adc
//
// Revision 1.1  2018/11/14 16:50:11  akio
// FCS codes in offline/upgrade/akio
//

#ifndef STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_
#define STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_

#include <map>
#include <vector>

#include "StMaker.h"

class StFcsCollection;
class StFcsHit;
class StFcsCluster;
class StFcsPoint;
class StFcsDbMaker;
class StMuDst;

class StFcsClusterMaker : public StMaker {
public:
    
    StFcsClusterMaker(const char* name = "StFcsClusterMaker");
    ~StFcsClusterMaker();
    Int_t InitRun(Int_t runNumber);
    Int_t Make();
    void Clear(Option_t* option = "");
    
    /** Set to read MuDST, then only this maker does is recalc point position using DB values */
    /** and does NOT perform cluster finding nor fitting */
    //void SetReadMuDst(int v=1) {mReadMuDst=v;} 
    
    void setDebug(int v) {mDebug=v;}
    void setEnergySelect(int v) {mEnergySelect=v;}
    void setSumTimeBins(int min, int max) {mMinTB=min; mMaxTB=max;}

    void set_NEIGHBOR_DISTANCE(float e, float h){m_NEIGHBOR_DISTANCE_Ecal=e; m_NEIGHBOR_DISTANCE_Hcal=h;}
    void set_DISTANCE_ADVANTAGE(float v){m_DISTANCE_ADVANTAGE=v;}
    void set_TOWER_E_THRESHOLD(float v){m_TOWER_E_THRESHOLD=v;}
    void set_TOWER_E_THRE_MOMENT(float v){m_TOWER_E_THRE_MOMENT=v;}
    void set_TOWER_E_RATIO2SPLIT(float v){m_TOWER_E_RATIO2SPLIT=v;}

 private:
    int makeSum(int det);
    int makeFit(int det);
    int makeCluster(int det);

    float isNeighbor(StFcsHit* hit,  StFcsCluster* clu);
    float distance(StFcsHit* hit1, StFcsHit* hit2);
    float distance(StFcsHit* hit, StFcsCluster* clu);
    void  updateCluster(StFcsCluster* clu);
    void  clusterMomentAnalysis(StFcsCluster* clu, float ecut=-1.0);
    float getSigma(StFcsCluster* clu, double thetam, float ecut);
    void   categorization(StFcsCluster* clu);
	
    StFcsDbMaker* mDb=0;               //!
    StFcsCollection* mFcsCollection=0; //!

    Int_t mDebug=0;                        //! debug opption
    Int_t mEnergySelect=0;                 //! 0=MC(straight from dE), 1=Sum of timebins, 2=Fit
    Int_t mMinTB=35;                       //! min timebin for sum
    Int_t mMaxTB=60;                       //! max timebin for sum
    Float_t m_NEIGHBOR_DISTANCE_Ecal=1.01; //! Distance to make it neignbor for Ecal and Hcal
    Float_t m_NEIGHBOR_DISTANCE_Hcal=1.42; //! 1.01 for 4 towers around
                                           //! 1.42 for 8 surrounding)
    Float_t m_DISTANCE_ADVANTAGE  = 1.2;   //! if similar distanced cluster found, higher E cluster takes it
    Float_t m_TOWER_E_THRESHOLD   = 0.01;  //! Tower E threshold for clustering [GeV]
    Float_t m_TOWER_E_THRE_MOMENT = 0.1;   //! Tower E threshold for moment analysis [GeV]
    Float_t m_TOWER_E_RATIO2SPLIT = 1.5;   //! Neighbor E * threshold > tower E to split cluster

    //Int_t readMuDst();
    //Int_t mReadMuDst=0;  //! 0= Do clustering
    //                     //! 1= Just read Mudst and recalc positions based on DB values

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
    ClassDef(StFcsClusterMaker, 0)
};
#endif  // STROOT_STFCSPOINTMAKER_STFCSPOINTMAKER_H_
