#include <math.h>
#include "StDstPointChair.h"

ClassImp(StDstPointChair)

//________________________________________________________________________________
Int_t StDstPointChair::DetectId(const Char_t *name) 
{
  //
  // return > 0 detector index by its name
  //        = 0 wrong detector name found
  //
  const Char_t *detectors[] = {"tpc",     "svt", "ftpc", "ssd" };
#if 0
                               "rich",    "ftpcWest", "ftpcEast","tof",
                               "ctb",     "ssd"};
                               "ftpcWest","ftpcEast","tof",
                               "ctb",     "ssd"};
  const Int_t index[] = { kUnknownIdentifier
                         ,kTpcIdentifier
                         ,kSvtIdentifier
                         ,kRichIdentifier
                         ,kFtpcWestIdentifier
                         ,kFtpcEastIdentifier
                         ,kTofPatchIdentifier
                         ,kCtbIdentifier
                         ,kSsdIdentifier
                        }
#endif

  Int_t l = sizeof(detectors)/sizeof(Char_t *);
  for (Int_t i=0; i < l ; i++) 
     if ( strcmp(name,detectors[i]) ) return i+1;
  return 0;
}
//________________________________________________________________________________
Float_t StDstPointChair::Factor(Float_t &range,Float_t &errF, Int_t detId) 
{

//     see: CC: FILE:       dst_point_filler.F (looked up on 24.01.2000 )
//      parameter(detid_tpc  = 1)
//      parameter(detid_svt  = 2)
//      parameter(detid_ftpc = 3)
//      parameter(detid_ssd  = 8) // == 0

  const Float_t factors[]   = {2380 , 23800 , 2380, 0, 0, 0, 0, 16000 };
  const Float_t ranges[]    = { 220 ,  22   ,  270, 0, 0, 0, 0,  40   };
  const Int_t   errFactor[] = { -17 , -26   ,    0, 0, 0, 0, 0, -26   };
  assert(detId <= Int_t(sizeof(factors)/sizeof(Float_t)));
  Float_t ret = 0;
  if (detId) {
    range = ranges[detId-1];
    ret   = factors[detId-1]; 
    errF  = ldexp(1.0,errFactor[detId-1]);
  } else {
    ret   = factors[1];
    range =  ranges[1];
    errF  =  ldexp(1.0,errFactor[1]);
  }
  return ret; 
}

