#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV1P0_Reader.hh"
#include "TPCV2P0_Reader.hh"

// Detector Factory

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
