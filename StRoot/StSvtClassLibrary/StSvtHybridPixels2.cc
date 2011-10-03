 /***************************************************************************
 *
 * $Id: StSvtHybridPixels2.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array used for 2nd order pedestal correction
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixels2.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// It is an array of 128 StSvtHybridPixels (capacitors),                      //
// where each one of these StSvtHybridPixels have 240 X 128 pixels (floats).  // 
// It is used to perform 2nd order pedestal correction, i.e.,                 //
// to calculate the pedestals given its pixel and SCA capacitor read out.     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "StArray.h"
#include "StSvtHybridPixels.hh"
#include "StSvtHybridPixels2.hh"

ClassImp(StSvtHybridPixels2)

StSvtHybridPixels2::StSvtHybridPixels2(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  // The same as StSvtHybridObject. It sets the numbr of SCA capacitors to 128

  mNumberOfCapacitors = 128;
  
  mPixels = new StObjArray(mNumberOfCapacitors);
}

StSvtHybridPixels2::~StSvtHybridPixels2()
{
  delete mPixels;
}


