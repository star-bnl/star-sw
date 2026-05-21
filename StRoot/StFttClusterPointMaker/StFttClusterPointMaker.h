#ifndef STFTTCLUSTERPOINTMAKER_H
#define STFTTCLUSTERPOINTMAKER_H
#include "StMaker.h"
#include <vector>
#include <map>
#include "StFttDbMaker/StFttDb.h"


// class StFttDb;
class StEvent;
class StFttCollection;
class StFttCluster;
class StFttPoint;

class StFttClusterPointMaker: public StMaker {

public:
    StFttClusterPointMaker( const char* name = "stgcClusterPoint" );

    ~StFttClusterPointMaker();


    Int_t  Init();
    Int_t  InitRun( Int_t );
    Int_t  FinishRun( Int_t );
    Int_t  Finish();
    Int_t  Make();

    void setUseGeantData( bool useGeantData ) { mUseGeantData = useGeantData; }

private:
    void InjectTestData();
    void MakeLocalPoints(UChar_t Rob);
    void MakeGlobalPoints();
    void MakeGeantPoints(); // act as a slow-sim
    //in the ClusterPointMaker there no ghost hit rejection since all the clusters will be saved

    StEvent*             mEvent;
    StFttCollection*     mFttCollection;
    Bool_t               mDebug;
    Bool_t               mUseTestData;
    Bool_t               mUseGeantData; // if true, use the geant hits to make points
    StFttDb*             mFttDb;
    std::vector<StFttCluster *> clustersPerRob[StFttDb::nRob][StFttDb::nStripOrientations]; //save the cluster for per quadrant

    inline bool is_Group1(int row_x, int row_y, double x, double y) const { return ( (14.60 <= x && x <= 172.29) && (14.60 <= y && y <= 172.29) && (row_x == 0) && (row_y == 0) ); }
    inline bool is_Group2(int row_x, int row_y, double x, double y) const { return ( (172.29 <= x && x <= 360.09) && (14.60 <= y && y <= 172.29) && (row_x == 0) && (row_y == 1)); }
    inline bool is_Group3(int row_x, int row_y, double x, double y) const
    {
        return ( ( ( (360.09 <= x && x <= 504.2) && (14.60 <= y && y <= 172.29) ) || ( (504.2<= x && x <= 548.3) && (14.60 <= y && y <= 216.89) ) ) && (row_x == 0) && (row_y == 2) );
    }
    inline bool is_Group4(int row_x, int row_y, double x, double y) const { return ( (14.60 <= x && x <= 172.29) && (172.29 <= y && y <= 360.09) && (row_x == 1) && (row_y == 0) ); }
    inline bool is_Group5(int row_x, int row_y, double x, double y) const
    {
        return ( ( ((172.29 <= x && x <= 315.4) && (172.29 <= y && y <= 360.09)) || ((315.4 <= x && x <= 360.09) && (172.29 <= y && y <= 410.9)) || ((360.09 <= x && x <= 410.9) && (315.4 <= y && y <= 410.9)) ) && (row_x == 1) && (row_y == 1) );
    }
    inline bool is_Group6(int row_x, int row_y, double x, double y) const { return ((360.09 <= x && x <= 504.2) && (172.29 <= y && y <= 315.4)) && (row_x == 1) && (row_y == 2); }
    inline bool is_Group7(int row_x, int row_y, double x, double y) const
    {
        return ( ( ( (360.09 <= y && y <= 504.2) && (14.60 <= x && x <= 172.29) ) || ( (504.2<= y && y <= 548.3) && (14.60 <= x && x <= 216.89) ) ) && (row_x == 2) && (row_y == 0));
    }
    inline bool is_Group8(int row_x, int row_y, double x, double y) const { return ( ((360.09 <= y && y <= 504.2) && (172.29 <= x && x <= 315.4)) && (row_x == 2) && (row_y == 1)); }

    ClassDef( StFttClusterPointMaker, 1 )
};

#endif