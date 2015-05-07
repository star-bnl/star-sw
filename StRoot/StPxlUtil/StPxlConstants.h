/*!
 * \class StPxlConstants
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlConstants.h,v 1.11 2015/05/07 20:58:20 smirnovd Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 * some constants for pxl detector
 *
 ***************************************************************************
 *
 * $Log: StPxlConstants.h,v $
 * Revision 1.11  2015/05/07 20:58:20  smirnovd
 * Defined dimensional constants for PXL sensitive sensor area
 *
 * Revision 1.10  2014/01/28 19:29:47  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlConstants_hh
#define StPxlConstants_hh

const int kNumberOfPxlSectors = 10;
const int kNumberOfPxlLaddersPerSector = 4;
const int kNumberOfPxlSensorsPerLadder = 10;
const int kNumberOfPxlColumnsOnSensor = 960;
const int kNumberOfPxlRowsOnSensor = 928;

namespace StPxlConsts
{

//@{
/**
 * Dimensions of PXL sensor active area. E.g. the values are used to restrict
 * smeared hits to active area see
 * StPxlSimMaker/doc/PXL_ultimate_sensor_flemming.pdf
 */
const double kPxlActiveLengthX = 1.921;   ///< in cm
const double kPxlActiveLengthY = 1.9872;  ///< in cm
//@}

}

#endif
