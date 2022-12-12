/***************************************************************************
* StFstConsts
* Author: Shenghui Zhang, Sep 2021
* Initial Revision
* Description: Hard-coded constants for the FST detector.
****************************************************************************/

#ifndef StFstConsts_h
#define StFstConsts_h

#include "StEvent/StEnumerations.h"
#include "TMath.h"
const int kFstNumWedges               = 36;  ///< 36 FST wedge
const int kFstNumSensorsPerWedge      = 3;   ///< 3 sensor per one FST Wedge
const int kFstNumInnerSensorsPerWedge = 1;   ///< 1 inner sensor per one FST Wedge
const int kFstNumOuterSensorsPerWedge = 2;   ///< 2 outer sensor per one FST Wedge
const int kFstNumSensors              = 108; ///< 3*36 sensors
const int kFstNumStripsPerInnerSensor = 512; ///< 128*4 strips in each inner sensor
const int kFstNumStripsPerOuterSensor = 256; ///< 64*4 strips in each outer sensor
const int kFstNumWedgePerDisk         = 12;  ///< 12 wedge per one FST Disk
const int kFstNumPhiSegPerWedge       = 128; ///< phi segmentation in r-phi direction per each wedge
const int kFstNumPhiSegPerSensor      = 128; ///< phi segmentation in r-phi direction per each sensor: 128 for sensor 1 | 64 for sensor 2 & 3
const int kFstNumRStripsPerSensor     = 4;   ///< 4 R-strips in r-phi direction per each sensor

/*
 * Hard-coded constants
 */
//electronics constants
const int kFstNumRdos            = 6;     // Rdos, numbering from 1, 2, 3, ..., 6
const int kFstNumArmsPerRdo      = 3;     // 3 ARMs covered per Rdo
const int kFstNumApvsPerArm      = 16;    // 16 APV chips covered by one ARM, numbering 0-7 & 12-19
const int kFstNumApvChannels     = 128;   // 128 readout channels per one APV chip
const int kFstNumChanPerArm      = 2048;  // 16*128 channels per ARM board
const int kFstNumElecIds         = 36864; // 8*36*128 channels, elec. ID numbering from 0 to 36863
const int kFstApvsPerWedge       = 8;     // 8 APV chips covered in one Wedge
const int kFstNumApvs            = 288;   // 36*8 APV chips in whole FST detector
const int kFstNumRStripsPerWedge = 8;     // 8 R-strips per each Wedge
const int kFstNumDisk            = 3;     // 3 Disk
const int kFstNumWedsPerRdo      = 6;     // 6 wedegs per Rdo
const int kFstNumWedsPerArm      = 2;     // 2 wedges per ARM

//FST strip dimenssion
const float kFstSensorActiveSizeR             = 5;  //cm, smallest radius of FST sensor active area in R direction
const float kFstStripPitchR                   = 2.875;      // cm, FST strip pitch in r direction is (28-16.5)/4. or (16.5-5)/4.
const float kFstStripPitchPhi                 = TMath::Pi()*30.0/180.0/kFstNumPhiSegPerSensor;  //rad, FST strip pitch in phi direction
const float kFstStripGapPhi                   = TMath::Pi()*1.0/180.0; //rad, 1 degree gap between 2 outer sensor of FST
const int   kFstzFilp[kFstNumDisk]            = {1,-1,1};
const int   kFstzDirct[kFstNumWedgePerDisk]   = {1,-1,1,-1,1,-1,1,-1,1,-1,1,-1};
const float kFstphiStart[kFstNumWedgePerDisk] = {2.0, 2.0, 0.0, 12.0, 10.0, 10.0, 8.0, 8.0, 6.0, 6.0, 4.0, 4.0}; // * pi/6
const float kFstphiStop[kFstNumWedgePerDisk]  = {3.0, 1.0, 1.0, 11.0, 11.0,  9.0, 9.0, 7.0, 7.0, 5.0, 5.0, 3.0}; // * pi/6
const float kFstrStart[kFstNumRStripsPerWedge]= {5.000, 7.875, 10.750, 13.625, 16.500, 19.375, 22.250, 25.125}; // in cm
const float kFstrStop[kFstNumRStripsPerWedge] = {7.875, 10.750, 136.25, 16.500, 19.375, 22.250, 25.125, 28.000}; // in cm

//general APV chip constants
const unsigned char kFstNumTimeBins = 3;    // 3 time bins for ADC sampling (maximum time bin number)
const unsigned char kFstDefaultTimeBin = 2; // the default time bin number (2nd time bin) for FST raw hits
const int kFstMaxAdc                = 4096; // ADC value should be less than 4096 (12 bits ADC)

#endif
