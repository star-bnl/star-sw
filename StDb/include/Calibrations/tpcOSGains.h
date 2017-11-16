//:Description: OuterSector Pad-level gain correction factors 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequency: as needed to account for runtime variations 
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Calibrations_tpc/Sector_##/tpcOSGains

#ifndef TPC_OUTERSECTORGAINS_H
#define TPC_OUTERSECTORGAINS_H

struct tpcOSGains {

  float gain[3942]; // Pad-to-Pad corrections

};

#endif

