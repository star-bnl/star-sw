#ifndef HH_EEDIMS
#define HH_EEDIMS
#define MaxTwCrateCh 128
#define MaxMapmtCrateCh 192
#define MinTwCrateID 1
#define MaxTwCrateID 6
#define MaxTwCrates (MaxTwCrateID- MinTwCrateID+1)
#define MinMapmtCrateID 64
#define MaxMapmtCrateID 111
#define MaxMapmtCrates (MaxMapmtCrateID- MinMapmtCrateID+1)
#define MaxAnyCrate (MaxMapmtCrateID+1)
#define MaxAnyCh (MaxMapmtCrateCh)
#define MaxSmdStrips 300 // per plain 
#define MaxSmdPlains 2 /* plain per sector */
#define MaxSectors 12 /* in the endcap */
#define MaxSubSec 5 /* in the sector */
#define MaxEtaBins 12 /* in the sector */
#define MaxPhiBins 60  /* in the Endcap */

#endif
