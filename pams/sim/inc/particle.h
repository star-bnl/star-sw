/* particle.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 Generated particle table
   Id: particle.idl,v 1.1 19980210 15:12:41 fisyak Exp  
   Log: particle.idl,v 
   Revision 1.1  19980210 15:12:41  fisyak
   Particle table
 

 */
#ifndef PARTICLE_H
#define PARTICLE_H
#define PARTICLE_SPEC \
"struct particle { \
	long isthep; \
	long idhep; \
	long jmohep[2]; \
	long jdahep[2]; \
	float phep[5]; \
	float vhep[4]; \
};"
typedef struct particle_st {
	long isthep; /* status code of the entry */
	long idhep; /* particle identity, accordingly to the PDG standard */
	long jmohep[2]; /* pointer(s) to position where the mother(s) stored */
	long jdahep[2]; /* pointers to position of the first/last daughter */
	float phep[5]; /* p4 and mass (GeV) */
	float vhep[4]; /* production vertex (mm) and time (mm/c) */
} PARTICLE_ST;
#endif /* PARTICLE_H */
