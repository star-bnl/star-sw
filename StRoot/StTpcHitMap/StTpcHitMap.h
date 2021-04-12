/***********************************************************************
 *
 * $Id: StTpcHitMap.h,v 1.00 $
 *
 * Author: QIU Hao   12/3/2008
 *
 ***********************************************************************
 *
 * Description: a map to transform and correct tpc hit position
 *
 ***********************************************************************
 */
#ifndef StTpcHitMap_H
#define StTpcHitMap_H
#include "TObject.h"

const int nSectors = 24;
const int nPadrows = 72;
const int nPadGrids = 30;
const int nTimeBucketGrids = 40;
const int nTimeBuckets = 400;
const double timeBucketGridSize = (double)nTimeBuckets/(double)nTimeBucketGrids;
class StTpcHitMover;
class StTpcDb;
class TDataSet;
class StMagUtilities;

class StTpcHitMap : public TObject
{
  
public:
  StTpcHitMap(StTpcDb* tpcDbIn);
  virtual ~StTpcHitMap () {};
  void calculateMap(TDataSet* runLog, StMagUtilities* mExB);
  void writeMap();
  void loadMap();
  void printMap();
  void mapping(double* xyz, int sector, int padRow,
               double pad, double timeBucket, double spaceCharge);

  double tpcHitMap[nSectors][nPadrows][nPadGrids+1][nTimeBucketGrids+1][3][2];
  //[sector-1][padRow-1][padGrid][timeBucketGrid][x/y/z][independent or propotional to luminosity]

protected:

  StTpcDb* tpcDb;
  double padGridSize[nPadrows];
  
  ClassDef(StTpcHitMap, 1);
    
};
    
#endif
