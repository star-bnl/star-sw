//:Description: Sector level timeoffset normalization factors
//:Synonyms::::
//:Source:
//:Update:
//:Update frequency: as needed to account for runtime variations 
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Calibrations_tpc/tpcTimeNorms

#ifndef TPC_SECTORTIMENORMS_H
#define TPC_SECTORTIMENORMS_H

struct tpcTimeNorms {

 float innerNorm[24]; // Sector-to-Sector corrections
 float outerNorm[24]; // Sector-to-Sector corrections

};

#endif


