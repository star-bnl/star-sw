/* eg_gener.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef EG_GENER_H
#define EG_GENER_H
#define EG_GENER_SPEC \
"struct eg_gener { \
	char eg_name[32]; \
	float eg_version; \
	float sqrts; \
	float b_max; \
	long east_a; \
	long east_z; \
	long west_a; \
	long west_z; \
};"
typedef struct eg_gener_st {
	char eg_name[32]; /*            event generator name                  */
	float eg_version; /* long     eg_rndm[2];                generator random numbers              */
	float sqrts; /* float    b_min;                     minimum impact parameter              */
	float b_max; /*bmaxim      maximum impact parameter              */
	long east_a; /*maproj      projectile 1 mass number              */
	long east_z; /*laproj      projectile 1 charge                   */
	long west_a; /*matarg      projectile 2 mass number              */
	long west_z; /* long     polarization_run[10];      to be defined                         */
} EG_GENER_ST;
#endif /* EG_GENER_H */
