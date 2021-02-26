// $Id: StFcsClusterMaker.h,v 1.13 2021/02/25 21:52:57 akio Exp $
// $Log: StFcsClusterMaker.h,v $
// Revision 1.13  2021/02/25 21:52:57  akio
// Int_t -> int
//
// Revision 1.12  2021/02/25 19:24:15  akio
// Modified for STAR code review (Hongwei)
//
// Revision 1.11  2020/12/17 21:18:01  akio
// Separate RATIO2SPLIT for ecal/hcal
//
// Revision 1.10  2020/09/03 19:42:24  akio
// moving sum & fit to StFcsWaveformFitMaker
//
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

#ifndef STROOT_STFCSCLUSTERMAKER_STFCSCLUSTERMAKER_H_
#define STROOT_STFCSCLUSTERMAKER_STFCSCLUSTERMAKER_H_

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
    int InitRun(int runNumber);
    int Make();
    void Clear(Option_t* option = "");
    
    void setDebug(int v) {SetDebug(v);}

    void set_NEIGHBOR_DISTANCE(float e, float h){m_NEIGHBOR_DISTANCE_Ecal=e; m_NEIGHBOR_DISTANCE_Hcal=h;}
    void set_DISTANCE_ADVANTAGE(float v){m_DISTANCE_ADVANTAGE=v;}
    void set_TOWER_E_THRESHOLD(float v){m_TOWER_E_THRESHOLD=v;}
    void set_TOWER_E_THRE_MOMENT(float v){m_TOWER_E_THRE_MOMENT=v;}
    void set_TOWER_E_RATIO2SPLIT(float e, float h){m_TOWER_E_RATIO2SPLIT_Ecal=e; m_TOWER_E_RATIO2SPLIT_Hcal=h;}

 private:
    //move those to StFcsWaveformFitMaker   
    //int makeSum(int det);
    //int makeFit(int det);

    int makeCluster(int det);

    float isNeighbor(StFcsHit* hit,  StFcsCluster* clu);
    float distance(StFcsHit* hit1, StFcsHit* hit2);
    float distance(StFcsHit* hit, StFcsCluster* clu);
    void  updateCluster(StFcsCluster* clu);
    void  clusterMomentAnalysis(StFcsCluster* clu, float ecut=-1.0);
    float getSigma(StFcsCluster* clu, double thetam, float ecut);
    void  categorization(StFcsCluster* clu);
	
    StFcsDbMaker* mDb=0;               //!
    StFcsCollection* mFcsCollection=0; //!

    float m_NEIGHBOR_DISTANCE_Ecal=1.01; //! Distance to make it neignbor for Ecal and Hcal
    float m_NEIGHBOR_DISTANCE_Hcal=2.01; //! 1.01 for 4 towers around
                                           //! 1.42 for 8 surrounding)

    float m_DISTANCE_ADVANTAGE  = 1.2;   //! if similar distanced cluster found, higher E cluster takes it
    float m_TOWER_E_THRESHOLD   = 0.01;  //! Tower E threshold for clustering [GeV]
    float m_TOWER_E_THRE_MOMENT = 0.1;   //! Tower E threshold for moment analysis [GeV]

    float m_TOWER_E_RATIO2SPLIT_Ecal = 1.5; //! Neighbor E * threshold > tower E to merge cluster, otherwise split
    float m_TOWER_E_RATIO2SPLIT_Hcal = 2.0; //! Neighbor E * threshold > tower E to merge cluster, otherwise split

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
    ClassDef(StFcsClusterMaker, 1)
};
#endif  // STROOT_STFCSCLUSTERMAKER_STFCSCLUSTERMAKER_H_
