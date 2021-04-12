/***********************************************************************
 *
 * $Id: StTpcHitMap.h,v 1.00 $
 *
 * Author: QIU Hao   12/3/2008
 *
 ***********************************************************************
 *
 * Description: a map to transform correct tpc hit position
 *
 ***********************************************************************
 */

#include "StTpcHitMap.h"
#include "TMath.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDbUtilities/StMagUtilities.h"
#include "TDataSet.h"
#include "StTpcDb/StTpcDb.h"

ClassImp(StTpcHitMap)

const double numberOfPads[nPadrows] = {
    52,  54,  56,  58,  60,  62,  62,  64,  66,  68,  70,  72,  74,  74,  76,
    78,  80,  82,  84,  86,  86,  88,  90,  92,  94,  96,  98,  98,  100, 102,
    104, 106, 108, 110, 110, 112, 114, 116, 118, 120, 98,  100, 102, 104, 106,
    106, 108, 110, 112, 112, 114, 116, 118, 120, 122, 122, 124, 126, 128, 128,
    130, 132, 134, 136, 138, 138, 140, 142, 144, 144, 144, 144};

// ----------------------------------------------------------------------------------
//   StTpcDb* tpcDbIn = gStTpcDb;
StTpcHitMap::StTpcHitMap(StTpcDb* tpcDbIn)
{
  for(int i=0; i<nPadrows; i++)
    padGridSize[i] = numberOfPads[i]/nPadGrids;
  tpcDb = tpcDbIn;
}

// ----------------------------------------------------------------------------------
// See below an example to get the inputs of calculateMap();
// TDataSet *runLog = tpcHitMover->GetDataBase("RunLog");
// Int_t mode = tpcHitMover->GetMode();
void StTpcHitMap::calculateMap(TDataSet* runLog, StMagUtilities* mExB)
{
  float x[3], xTemp[3];
  const double spaceChargeToUse = 0.01;
  // Int_t option = (TMath::Abs(mode) & 0x7FFFFFFE) >> 1;
  // StMagUtilities* mExB = new StMagUtilities(tpcDb, runLog, option);
  StTpcCoordinateTransform transform(tpcDb);
  for(int iSpaceCharge=0; iSpaceCharge<2; iSpaceCharge++) {
    double spaceCharge = spaceChargeToUse*iSpaceCharge;
    mExB->ManualSpaceChargeR2(spaceCharge, mExB->CurrentSpaceChargeEWRatio()); 
    for(int iSector=0; iSector<nSectors; iSector++)
      for(int iPadRow=0; iPadRow<nPadrows; iPadRow++)
        for(int iPadGrid=0; iPadGrid<nPadGrids+1; iPadGrid++)
          for(int iTimeBucketGrid=0; iTimeBucketGrid<nTimeBucketGrids+1; iTimeBucketGrid++) {
            int        sector = iSector+1;
            int        padRow = iPadRow+1;
            double        pad = (double)iPadGrid*padGridSize[iPadRow];
            double timeBucket = (double)iTimeBucketGrid*timeBucketGridSize;
            StTpcPadCoordinate padCoord(sector, padRow, pad, timeBucket);

            StTpcLocalSectorCoordinate localSectorCoord;
            transform(padCoord, localSectorCoord, kFALSE);

#if 0
            StTpcLocalSectorAlignedCoordinate localSectorAlignedCoord;
            transform(localSectorCoord, localSectorAlignedCoord); // sector alignment

            StTpcLocalCoordinate localCoord;
            transform(localSectorAlignedCoord, localCoord);
            //	transform(localSectorCoord, localCoord);
#else
            StTpcLocalCoordinate localCoord;
            transform(localSectorCoord, localCoord);
#endif
            xTemp[0]=localCoord.position().x();
            xTemp[1]=localCoord.position().y();
            xTemp[2]=localCoord.position().z();
            mExB->UndoDistortion(xTemp, x, localCoord.fromSector());   // distortion correction

            StThreeVector<double> tempVector(x[0],x[1],x[2]);
            //	StThreeVector<double> tempVector(xTemp[0],xTemp[1],xTemp[2]);
            localCoord.setPosition(tempVector);

            StGlobalCoordinate globalCoord;
            transform(localCoord, globalCoord);
            x[0]=globalCoord.position().x();
            x[1]=globalCoord.position().y();
            x[2]=globalCoord.position().z();
	
            //fill map
            if(!iSpaceCharge)
              for(int j=0; j<3; j++)
                tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][0] = x[j];
            else
              for(int j=0; j<3; j++)
                tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][1] =
                  (x[j] - tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][0]) / spaceChargeToUse;
          }
  }
  delete mExB;
}

// ----------------------------------------------------------------------------------
void StTpcHitMap::loadMap()
{
  /*
  ifstream ifstr(fileName);
  for(int iSector=0; iSector<24; iSector++)
    for(int iPadRow=0; iPadRow<45; iPadRow++)
      for(int iLocalXGrid=0; iLocalXGrid<nLocalXGrids+1; iLocalXGrid++)
	for(int iLocalZGrid=0; iLocalZGrid<nLocalZGrids+1; iLocalZGrid++)
	  for(int iComponent=0; iComponent<4; iComponent++)
	    ifstr>>tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][iComponent];
  */
  char fileName[256];
  for(int i=0; i<24; i++)
    {
      sprintf(fileName, "tpcHitMap_sector%d.bin", i+1); 
      FILE* f1 = fopen(fileName, "r");
      fread(tpcHitMap[i], sizeof(tpcHitMap)/24, 1, f1);
    }

}

// ----------------------------------------------------------------------------------
void StTpcHitMap::writeMap()
{
  /*
    ofstream ofstr(fileName);
    for(int iSector=0; iSector<24; iSector++)
    for(int iPadRow=0; iPadRow<45; iPadRow++)
    for(int iLocalXGrid=0; iLocalXGrid<nLocalXGrids+1; iLocalXGrid++)
    for(int iLocalZGrid=0; iLocalZGrid<nLocalZGrids+1; iLocalZGrid++)
    for(int iComponent=0; iComponent<4; iComponent++)
    ofstr<<setw(20)<<tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][iComponent];
  */
  char fileName[256];
  for(int i=0; i<24; i++) {
    sprintf(fileName, "tpcHitMap_sector%d.bin", i+1); 
    FILE* f1 = fopen(fileName, "w");
    fwrite(tpcHitMap[i], sizeof(tpcHitMap)/24, 1, f1);
  }
}

// ----------------------------------------------------------------------------------
void StTpcHitMap::printMap() {
    // float x0[3];
    // double localX, localY, localZ;
    // cout << setw(15) << "sector" << setw(15) << "padrow" << setw(15) << "lXGrid" << setw(15) << "lZGrid" << setw(15)
    //      << "x" << setw(15) << "y" << setw(15) << "z" << setw(15) << "dx0" << setw(15) << "dy0" << setw(15) << "dx1"
    //      << setw(15) << "dy1" << endl;
    // for (int iSector = 0; iSector < 24; iSector++)
    //     for (int iPadRow = 0; iPadRow < 45; iPadRow++)
    //         for (int iLocalXGrid = 0; iLocalXGrid < nLocalXGrids + 1; iLocalXGrid++)
    //             for (int iLocalZGrid = 0; iLocalZGrid < nLocalZGrids + 1; iLocalZGrid++) {
    //                 localX = ((double)iLocalXGrid - (double)nLocalXGrids / 2.) * localXGridSize[iPadRow];
    //                 localY = padRowLocalY[iPadRow];
    //                 localZ = iLocalZGrid * localZGridSize;
    //                 StTpcCoordinateTransform transform(tpcDb);
    //                 StGlobalCoordinate global;
    //                 StTpcLocalSectorCoordinate localSector(localX, localY, localZ, iSector + 1, iPadRow + 1);
    //                 transform(localSector, global);
    //                 x0[0] = global.position().x();
    //                 x0[1] = global.position().y();
    //                 x0[2] = global.position().z();
    //                 cout << iSector + 1 << setw(15) << iPadRow + 1 << setw(15) << iLocalXGrid << setw(15) << iLocalZGrid
    //                      << setw(15) << x0[0] << setw(15) << x0[1] << setw(15) << x0[2] << setw(15)
    //                      << tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][0] << setw(15)
    //                      << tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][1] << setw(15)
    //                      << tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][2] << setw(15)
    //                      << tpcHitMap[iSector][iPadRow][iLocalXGrid][iLocalZGrid][3] << endl;
    //             }
}

// ----------------------------------------------------------------------------------
void StTpcHitMap::mapping(double* xyz, int sector, int padRow, double pad, double timeBucket, double spaceCharge)
{
  int iSector = sector-1;
  int iPadRow = padRow-1;
  double padGrid = pad/padGridSize[iPadRow];
  double timeBucketGrid = timeBucket/timeBucketGridSize;
  if(padGrid<0 || padGrid>(double)nPadGrids+1. || timeBucketGrid<0) {
    cout << "StTpcHitMap:WARN  - StTpcHitMap::mapping(): out of grids. padGrid:" << padGrid
         <<" timeBucketGrid:" << timeBucketGrid << endl;
    xyz[0] = 10000.;
    xyz[1] = 10000.;
    xyz[2] = 10000.;
    return;
  }	  
  int iPadGrid = (int)padGrid;
  int iTimeBucketGrid = (int)timeBucketGrid;
  if(iTimeBucketGrid < nTimeBucketGrids) {
    double dPad = padGrid-iPadGrid;
    double dTimeBucket = timeBucketGrid-iTimeBucketGrid;
    double xyz00[3], xyz01[3], xyz10[3], xyz11[3];
    for(int j=0; j<3; j++) {
      xyz00[j] = tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][0]     + tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][1] * spaceCharge;
      xyz01[j] = tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid+1][j][0]   + tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid+1][j][1] * spaceCharge;
      xyz10[j] = tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid][j][0]   + tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid][j][1] * spaceCharge;
      xyz11[j] = tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid+1][j][0] + tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid+1][j][1] * spaceCharge;

      xyz[j] = xyz00[j]*(1-dPad)*(1-dTimeBucket) + xyz01[j]*(1-dPad)*dTimeBucket  + xyz10[j]*dPad*(1-dTimeBucket) + xyz11[j]*dPad*dTimeBucket;
    }
  } else {    // if(iTimeBucketGrid >= nTimeBucketGrids)
    iTimeBucketGrid = nTimeBucketGrids;
    double dPad = padGrid-iPadGrid;
    double xyz0[3], xyz1[3];
    for(int j=0; j<3; j++) {
      xyz0[j] = tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][0] + tpcHitMap[iSector][iPadRow][iPadGrid][iTimeBucketGrid][j][1] * spaceCharge;
      xyz1[j] = tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid][j][0] + tpcHitMap[iSector][iPadRow][iPadGrid+1][iTimeBucketGrid][j][1] * spaceCharge;
      xyz[j] = xyz0[j]*(1-dPad) + xyz1[j]*dPad;
    }
  }
}
