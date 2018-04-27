/***********************************************************************
 *
 * $Id: online_tracking_TpcHitMap.cxx,v 1.1 2013/09/13 21:09:15 fisyak Exp $
 *
 * Author: QIU Hao   12/3/2008
 * modified by Hongwei Ke 1/10/2011
 *
 ***********************************************************************
 *
 * Description: a map to correct tpc hit distortion
 *
 ***********************************************************************
 */

#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <stdio.h>	// for FILE ops
#include <errno.h>
#include <cmath>

using namespace std;

// #include "gl3TriggerDecider.h"
// #include "online_tracking_collector.h"
// #include <rtsLog.h>

#include "online_tracking_TpcHitMap.h"

const double numberOfPads[45] = 
    {88,96,104,112,118,126,134,142,150,158,166,174,182,
     98,100,102,104,106,106,108,110,112,112,114,116,
     118,120,122,122,124,126,128,128,130,132,134,136,
     138,138,140,142,144,144,144,144
    };
  
//   StTpcDb* tpcDbIn = gStTpcDb;
online_tracking_TpcHitMap::online_tracking_TpcHitMap(const char* HLTparameters, int in_sectorID)
{
    spaceChargeP0 = -4.9e-4;
    spaceChargeP1 = 7.45e-8;
    spaceChargeP2 = 0.;
    spaceCharge   = 0.;
    scalerToUse   = 3;
    sectorID      = in_sectorID;
    driftVelocity = 0;

    use8rowSpaceChargeMethod = 0;
    for(int i=0; i<8; i++)
      {
	detector[i] = 0;
	saturation[i] = 0;
	correction[i] = 0;
	factor[i] = 0;
	offset[i] = 0;
      }

    readHLTparameters(HLTparameters);
  
    for(int i=0; i<45; i++)
        padGridSize[i] = numberOfPads[i]/nPadGrids;
}

void online_tracking_TpcHitMap::readHLTparameters(const char* HLTparameters)
{
    string parameterName;
    ifstream ifs(HLTparameters);
    if(!ifs.fail())
        while(!ifs.eof()) {
            ifs >> parameterName;
            if (parameterName == "spaceChargeP0")    ifs >> spaceChargeP0;
            if (parameterName == "spaceChargeP1")    ifs >> spaceChargeP1;
            if (parameterName == "spaceChargeP2")    ifs >> spaceChargeP2;
            if (parameterName == "scalerToUse")      ifs >> scalerToUse;
            if (parameterName == "mapDriftVelocity") ifs >> mapDriftVelocity;
	    if (parameterName == "8rowSpaceCharge")
	      {
		use8rowSpaceChargeMethod = 1;
		for(int i=0; i<8; i++)
		  ifs>>detector[i]>>saturation[i]>>correction[i]>>factor[i]>>offset[i];
	      }
        }
}
  
void online_tracking_TpcHitMap::setSpaceCharge(double sc)
{
    spaceCharge = sc;
}

void online_tracking_TpcHitMap::setSpaceChargeFromScalerCount(double scalerCount)
{
    spaceCharge = spaceChargeP0 + spaceChargeP1*scalerCount + spaceChargeP2*scalerCount*scalerCount;
}

void online_tracking_TpcHitMap::setSpaceChargeFromScalerCounts(int *scalerCounts)
{
  if(!use8rowSpaceChargeMethod)
    {
      int scalerCount = 0;
      switch (scalerToUse)
        {
        case 1: scalerCount = scalerCounts[2]; break;
        case 2: scalerCount = scalerCounts[7]; break;
        case 3: scalerCount = scalerCounts[5]+scalerCounts[6]; break;
        case 4: scalerCount = scalerCounts[0]+scalerCounts[1]; break;
        default: break;
        }      
    setSpaceChargeFromScalerCount(scalerCount);
    }
  else // 8 row method
    {
      double coulombs = 0;
      for(int i=0; i<8; i++)
	{
	  double mult = 0;
	  switch (detector[i])
	    {
	    case 1: mult = scalerCounts[2]; break;
	    case 2: mult = scalerCounts[7]; break;
	    case 3: mult = scalerCounts[5]+scalerCounts[6]; break;
	    case 4: mult = scalerCounts[0]+scalerCounts[1]; break;
	    case 5: mult = scalerCounts[5]; break;
	    case 6: mult = scalerCounts[6]; break;
	    case 7: mult = scalerCounts[0]; break;
	    case 8: mult = scalerCounts[1]; break;
	    case 9: mult = scalerCounts[3]; break;
	    case 10: mult = scalerCounts[4]; break;
	    default: break;
	    }      
	  double intens = (mult < saturation[i]) ? mult : saturation[i];
	  coulombs += ::pow(intens-offset[i],factor[i]) * correction[i] ;
	}
      spaceCharge = coulombs;
    }
}

void online_tracking_TpcHitMap::loadMap(const char* fileName)
{
    FILE* f1 = fopen(fileName, "r");
    if(f1) {
	fread(tpcHitMap, sizeof(tpcHitMap), 1, f1);
	fclose(f1);
    } else {
	// LOG(ERR,"Can't load map file \"%s\" [%s]",fileName,strerror(errno)) ;
      std::cerr << "Can't load map file " << fileName << " " << strerror(errno) << std::endl;
    }

}

void online_tracking_TpcHitMap::setDriftVelocity(double inDriftVelocity) {
    driftVelocity = inDriftVelocity;
}

void online_tracking_TpcHitMap::mapping(double* xyz, int padRow, double pad, double timeBucket)
{
    int    iPadRow        = padRow-1;
    double padGrid        = pad/padGridSize[iPadRow];
    double timeBucketGrid = timeBucket/timeBucketGridSize;
    if(padGrid<0 || padGrid>(double)nPadGrids+1. || timeBucketGrid<0) {
        cout<<"StTpcHitMap:WARN  - StTpcHitMap::mapping(): out of grids. padGrid:"<<padGrid<<" timeBucketGrid:"<<timeBucketGrid<<endl;
        xyz[0] = 10000.;
        xyz[1] = 10000.;
        xyz[2] = 10000.;
        return;
    }	  
    int iPadGrid = (int)padGrid;
    int iTimeBucketGrid = (int)timeBucketGrid;
    if(iTimeBucketGrid <= nTimeBucketGrids) {
        double dPad = padGrid-iPadGrid;
        double dTimeBucket = timeBucketGrid-iTimeBucketGrid;
        double xyz00[3], xyz01[3], xyz10[3], xyz11[3];
        for(int j=0; j<3; j++) {
            xyz00[j] = tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid][j][0] + tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid][j][1] * spaceCharge;
            xyz01[j] = tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid+1][j][0] + tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid+1][j][1] * spaceCharge;
            xyz10[j] = tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid][j][0] + tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid][j][1] * spaceCharge;
            xyz11[j] = tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid+1][j][0] + tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid+1][j][1] * spaceCharge;
            xyz[j]   = xyz00[j]*(1-dPad)*(1-dTimeBucket) + xyz01[j]*(1-dPad)*dTimeBucket + xyz10[j]*dPad*(1-dTimeBucket) + xyz11[j]*dPad*dTimeBucket;
        }
    } else {                 // if(iTimeBucketGrid > nTimeBucketGrids)
        iTimeBucketGrid = nTimeBucketGrids+1;
        double dPad = padGrid-iPadGrid;
        double xyz0[3], xyz1[3];
        for(int j=0; j<3; j++) {
            xyz0[j] = tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid][j][0] + tpcHitMap[iPadRow][iPadGrid][iTimeBucketGrid][j][1] * spaceCharge;
            xyz1[j] = tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid][j][0] + tpcHitMap[iPadRow][iPadGrid+1][iTimeBucketGrid][j][1] * spaceCharge;
            xyz[j]  = xyz0[j]*(1-dPad) + xyz1[j]*dPad;
        }
    }

    // use drift velocity to correct coordinate z
    double gatedGrid2Pad[2] = {1.0, 1.4}; // in cm
    double driftDistance    = 208.707;    // in cm

    int flagInnerOuter = (iPadRow  < 13 ?  0 : 1); // 0 <= iPadRow <= 44
    int flagEastWest   = (sectorID > 12 ? -1 : 1); // 0 <= sectorID <= 23, east (12-23, z<0) or west (0-11, z>0)

    double zMap = xyz[2];
    double dMap = driftDistance - flagEastWest * zMap;
    // printf(">>> driftVelocity = %10.5f mapDriftVelocity = %10.5f\n", driftVelocity, mapDriftVelocity);
    // printf("  HLT in Map: %10.4f %10.4f %10.4f\n", xyz[0], xyz[1], xyz[2]);
    xyz[2] = (driftDistance - (dMap + gatedGrid2Pad[flagInnerOuter]) * driftVelocity / mapDriftVelocity + gatedGrid2Pad[flagInnerOuter]) * flagEastWest;
}
