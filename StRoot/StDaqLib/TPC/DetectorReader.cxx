/***************************************************************************
 * $Id: DetectorReader.cxx,v 1.2 1999/07/02 04:43:22 levine Exp $
 * Author: Jeff Landgraf
 ***************************************************************************
 * Description:  Detector Factory
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: DetectorReader.cxx,v $
 * Revision 1.2  1999/07/02 04:43:22  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/
#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV1P0_Reader.hh"
#include "TPCV2P0_Reader.hh"



DetectorReader *getDetectorReader(EventReader *er, string det)
{
  DetectorReader *dr;

  if(det == "TPC")
    {
      dr = new TPCV1P0_Reader(er);
    }
  else if (det == "TPCV1P0")
    {
      dr = new TPCV1P0_Reader(er);
    }
   else if (det == "TPCV2P0")
     {
       dr = new TPCV2P0_Reader(er);
     }
  else
  {
    dr = NULL;
  }

  return dr;
}
