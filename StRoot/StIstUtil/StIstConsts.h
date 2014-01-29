/***************************************************************************
* StIstConsts
* Author: Yaping Wang, Jan 2013
* Initial Revision
* Description: Hard-coded constants for the IST detector.
****************************************************************************/

#ifndef StIstConsts_h
#define StIstConsts_h

/*
 * Hard-coded constants
 */
//electronics constants
#define kIstNumRdos                 6       // Rdos, numbering from 1, 2, 3, ..., 6
#define kIstNumArmsPerRdo           6       // 6 ARMs covered per Rdo
#define kIstNumApvsPerArm           24      // 24 APV chips covered by one ARM
#define kIstNumApvChannels          128     // 128 readout channels per one APV chip
#define kIstNumChanPerArm	    3072    // 3072 channels per ARM board
#define kIstNumElecIds              110592  // 110592 channels, elec. ID numbering from 0 to 110591
#define kIstApvsPerSection          12      // 12 APV chips covered by one section of a Ladder
#define kIstApvsPerLadder           36      // 36 APV chips covered in one Ladder
#define kIstNumApvs		    864     // 864 APV chips in whole IST detector

//physical constants
#define kIstNumLadders              24      // 24 IST Ladders
#define kIstNumSensorsPerLadder     6       // 6 sensor per one IST Ladder
#define kIstNumSensors		    144      // 144 sensors
#define kIstNumPadsPerSensor        768     // 768 pads in each sensor
#define kIstNumRowsPerSensor        64      // 64 rows in r-phi direction per each sensor
#define kIstNumColumnsPerSensor     12      // 12 columns in beam direction per each sensor

//IST pad dimenssion
#define kIstSensorActiveSizeRPhi    3.8016   // IST sensor active area in R-Phi direction
#define kIstSensorActiveSizeZ       7.5300   // IST sensor active area in Z direction
#define kIstPadPitchRow             0.0594   // IST pad dimension in r-phi direction is 0.0596 cm
#define kIstPadPitchColumn          0.6275   // IST pad dimension in beam direction is 0.6275 cm

//general APV chip constants
#define kIstNumTimeBins             9       // 9 time bins for ADC sampling (maximum time bin number)
#define kIstMaxAdc                  4096    // ADC value should be less than 4096 (12 bits ADC)

//IST space resolution
#define kIstPosResolutionRPhi	    0.0163   // IST position resolution in R-Phi direction (cm) w/ cluster size 1
#define kIstPosResolutionRPhi2      0.0200   // IST position resolution in R-Phi direction (cm) w/ cluster size 2
#define kIstPosResolutionZ	    0.2325   // IST position resolution in Z direction (cm)

#endif
