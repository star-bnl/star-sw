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
  const Char_t *detectors[] = {"tpc",     "svt", 
                               "rich",    "ftpcWest", "ftpcEast","tof",
                               "ctb",     "ssd"};
#if 0
 $STAR/include/StDetectorDefinitions.h:
 -----------------------------------------
# define kUnknownIdentifier             0
# define kTpcIdentifier                 1
# define kSvtIdentifier                 2
# define kRichIdentifier                3
# define kFtpcWestIdentifier            4
# define kFtpcEastIdentifier            5
# define kTofPatchIdentifier            6
# define kCtbIdentifier                 7
# define kSsdIdentifier                 8
# define kBarrelEmcTowerIdentifier      9
# define kBarrelEmcPreShowerIdentifier 10
# define kBarrelSmdEtaStripIdentifier  11
# define kBarrelSmdPhiStripIdentifier  12
# define kEndcapEmcTowerIdentifier     13
# define kEndcapEmcPreShowerIdentifier 14
# define kEndcapSmdUStripIdentifier    15
# define kEndcapSmdVStripIdentifier    16
# define kZdcWestIdentifier            17
# define kZdcEastIdentifier            18
# define kMwpcWestIdentifier           19
# define kMwpcEastIdentifier           20
# define kTpcSsdIdentifier             21
# define kTpcSvtIdentifier             22
# define kTpcSsdSvtIdentifier          23
# define kSsdSvtIdentifier             
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
//      parameter(detid_tpc  = kTpcIdentifier = 1)
//      parameter(detid_svt  = kSvtIdentifier = 2)
//      parameter(detid_ftpc = 3)
//      parameter(detid_ssd  = kSsdIdentifier = 8) // == 0

  const Float_t factors[]   = {2380 , 23800 , 2380, 2380, 2380, 0, 0, 16000 };
  const Float_t ranges[]    = { 220 ,  22   ,  270,  270,  270, 0, 0,  40   };
  const Int_t   errFactor[] = { -17 , -26   ,    0,  -17,  -17, 0, 0, -26   };
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

