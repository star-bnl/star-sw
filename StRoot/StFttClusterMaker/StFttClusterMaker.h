/***************************************************************************
 * StFttClusterMaker.h
 * Author: Daniel Brandenburg Feb, 2022
 ***************************************************************************/
#ifndef STFTTCLUSTERMAKER_H
#define STFTTCLUSTERMAKER_H
#include "StMaker.h"
#include <vector>
#include <map>



class StFttDb;
class StEvent;
class StFttCollection;
class StFttRawHit;
class StFttCluster;

class StFttClusterMaker: public StMaker {

public:
    StFttClusterMaker( const char* name = "stgcCluster" );

    ~StFttClusterMaker();


    int  Init();
    int  InitRun( int );
    int  FinishRun( int );
    int  Finish();
    int  Make();

private:
    void ApplyHardwareMap();
    std::vector<StFttCluster*> FindClusters( std::vector<StFttRawHit * > );
    StFttRawHit * FindMaxAdc( std::vector<StFttRawHit *>, size_t &pos );

    void InjectTestData();
    void SearchClusterEdges( std::vector< StFttRawHit * > hits, 
                                            size_t start, // start index at MaxADC
                                            size_t &left, size_t &right);
    void CalculateClusterInfo( StFttCluster * clu );

    // selection of raw hits for cluster building
    float GetThresholdFor( StFttRawHit * hit ) { return 0.0;}
    bool PassTimeCut( StFttRawHit * hit );

    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    int                  mRunYear;
    bool                 mDebug;
    StFttDb*             mFttDb;


    ClassDef( StFttClusterMaker, 1 )
};

#endif // STFTTCLUSTERMAKER_H