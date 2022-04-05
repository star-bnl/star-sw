/*!
 * \class StPxlConstants
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlConstants.h,v 1.12 2015/05/14 18:57:52 smirnovd Exp $
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
 * Revision 1.12  2015/05/14 18:57:52  smirnovd
 * Squashed commit of the following:
 *
 * StPxlFastSim: Streamlined creation of PXL hits by making use of StPxlUtil/StPxlDigiHit
 *
 * StPxlHitMaker: Updated comments
 *
 * StPxlHitMaker: Streamlined creation of PXL hits by making use of StPxlUtil/StPxlDigiHit
 *
 * StPxlDigiHit: A helper to manipulate local hit position in StPxlHit
 *
 * StPxlConsts: Define constants in namespace
 *
 * For safety reasons, the intentions is to move the constants into the namespace
 * and get rid of those defined in the global space.
 *
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

const double kPixelSize = 20.7e-4;       ///< Nominal size of a pixel in the PXL detector, in cm. Must be greater than 0
const int kPxlNumColumnsPerSensor = 960; ///< Number of pixels in a single row of a PXL sensor
const int kPxlNumRowsPerSensor = 928;    ///< Number of pixels in a single column of a PXL sensor

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
