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
const int kIstNumRdos = 6;       	// Rdos, numbering from 1, 2, 3, ..., 6
const int kIstNumArmsPerRdo = 6;       	// 6 ARMs covered per Rdo
const int kIstNumApvsPerArm = 24;      	// 24 APV chips covered by one ARM
const int kIstNumApvChannels = 128;     // 128 readout channels per one APV chip
const int kIstNumChanPerArm = 3072;    	// 3072 channels per ARM board
const int kIstNumElecIds = 110592;  	// 110592 channels, elec. ID numbering from 0 to 110591
const int kIstApvsPerSection = 12;      // 12 APV chips covered by one section of a Ladder
const int kIstApvsPerLadder = 36;      	// 36 APV chips covered in one Ladder
const int kIstNumApvs = 864;     	// 864 APV chips in whole IST detector

//IST pad dimenssion
const float kIstSensorActiveSizeRPhi = 3.8016;  // IST sensor active area in R-Phi direction
const float kIstSensorActiveSizeZ = 7.5300;   	// IST sensor active area in Z direction
const float kIstPadPitchRow = 0.0594;   	// IST pad dimension in r-phi direction is 0.0596 cm
const float kIstPadPitchColumn = 0.6275;   	// IST pad dimension in beam direction is 0.6275 cm

//general APV chip constants
const unsigned char kIstNumTimeBins = 9;       		// 9 time bins for ADC sampling (maximum time bin number)
const int kIstMaxAdc = 4096;    		// ADC value should be less than 4096 (12 bits ADC)

//IST space resolution
const float kIstPosResolutionRPhi = 0.0163;   	// IST position resolution in R-Phi direction (cm) w/ cluster size 1
const float kIstPosResolutionRPhi2 = 0.0200;   	// IST position resolution in R-Phi direction (cm) w/ cluster size 2
const float kIstPosResolutionZ = 0.2325;   	// IST position resolution in Z direction (cm)

#endif
