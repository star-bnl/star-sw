//:Description: Sector level gain normalization factors
//:Synonyms::::
//:Source:
//:Update:
//:Update frequency: as needed to account for runtime variations 
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Calibrations_tpc/tpcGainNorms

#ifndef TPC_SECTORGAINNORMS_H
#define TPC_SECTORGAINNORMS_H

struct tpcGainNorms {

 float innerNorm[24]; // Sector-to-Sector corrections
 float outerNorm[24]; // Sector-to-Sector corrections

};

#endif


