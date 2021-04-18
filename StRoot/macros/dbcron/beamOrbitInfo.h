/* beamOrbitInfo.h */
/* This file was made by the idl compiler "stic". Do not edit.
** This was generated for version '(unspecified)'
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 beamOrbitInfo.idl

 Table: beamOrbitInfo

 description: beam Orbit information


 */
#ifndef BEAMORBITINFO_H
#define BEAMORBITINFO_H
#define BEAMORBITINFO_SPEC \
"struct beamOrbitInfo { \
	unsigned long runNumber; \
	long blue_beamPos5_horizontal; \
	long blue_beamPos5_vertical; \
	long yellow_beamPos5_horizontal; \
	long yellow_beamPos5_vertical; \
	long blue_beamPos6_horizontal; \
	long blue_beamPos6_vertical; \
	long yellow_beamPos6_horizontal; \
	long yellow_beamPos6_vertical; \
	long blue_filledBuckets; \
	long yellow_filledBuckets; \
};"
typedef struct beamOrbitInfo_st {
	unsigned int runNumber; 
	int blue_beamPos5_horizontal; 
	int blue_beamPos5_vertical; 
	int yellow_beamPos5_horizontal; 
	int yellow_beamPos5_vertical; 
	int blue_beamPos6_horizontal; 
	int blue_beamPos6_vertical; 
	int yellow_beamPos6_horizontal; 
	int yellow_beamPos6_vertical; 
	int blue_filledBuckets; 
	int yellow_filledBuckets; 
} BEAMORBITINFO_ST;
#endif /* BEAMORBITINFO_H */
