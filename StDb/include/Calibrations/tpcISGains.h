//:Description: InnerSector Pad-level gain correction factors
//:Synonyms::::
//:Source:
//:Update:
//:Update frequency: as needed to account for runtime variations 
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Calibrations_tpc/Sector_##/tpcISGains
#ifndef TPC_INNERSECTORGAINS_H
#define TPC_INNERSECTORGAINS_H

struct tpcISGains {

  float gain[1750]; // Pad-to-Pad corrections

};

#endif
