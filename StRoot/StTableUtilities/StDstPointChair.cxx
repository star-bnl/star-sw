#include <math.h>
#include "StDstPointChair.h"
#include "StDetectorDefinitions.h"

ClassImp(StDstPointChair)

//________________________________________________________________________________
Int_t StDstPointChair::DetectId(const Char_t *name) 
{
  //
  // return > 0 detector index by its name
  //        = 0 wrong detector name found
  //
#if 0
//       $STAR/include/StDetectorDefinitions.h:
#define kUnknownIdentifier             0
#define kTpcIdentifier                 1
#define kSvtIdentifier                 2
#define kRichIdentifier                3
#define kFtpcWestIdentifier            4
#define kFtpcEastIdentifier            5
#define kTofIdentifier                 6
#define kCtbIdentifier                 7
#define kSsdIdentifier                 8
#define kBarrelEmcTowerIdentifier      9
#define kBarrelEmcPreShowerIdentifier 10
#define kBarrelSmdEtaStripIdentifier  11
#define kBarrelSmdPhiStripIdentifier  12
#define kEndcapEmcTowerIdentifier     13
#define kEndcapEmcPreShowerIdentifier 14
#define kEndcapSmdUStripIdentifier    15
#define kEndcapSmdVStripIdentifier    16
#define kZdcWestIdentifier            17
#define kZdcEastIdentifier            18
#define kMwpcWestIdentifier           19
#define kMwpcEastIdentifier           20
#define kTpcSsdIdentifier             21
#define kTpcSvtIdentifier             22
#define kTpcSsdSvtIdentifier          23
#define kSsdSvtIdentifier             24
 -----------------------------------------
#endif //0

  const Char_t *detectors[] = {"tpc", "svt", "rich", "ftpcWest" ,"ftpcEast"
                              ,"tof", "ctb", "ssd", 0};

  for (Int_t i=0; detectors[i] ; i++) 
     if ( strcmp(name,detectors[i]) ) return i+1;
  return 0;
}
//________________________________________________________________________________
Float_t StDstPointChair::Factor(Float_t &range,Float_t &errF, Int_t detId) 
{

  const Float_t factors  [] = {2380 , 23800 , 2380, 2380, 2380, 0, 0, 16000 };
  const Float_t ranges   [] = { 220 ,  22   ,  270,  270,  270, 0, 0,  40   };
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

