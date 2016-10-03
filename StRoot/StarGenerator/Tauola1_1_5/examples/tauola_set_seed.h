// Temporary solution embedded in this tauola_seed_seed.h:
//
// enables reinitialization of Fortran Random numbergenerator used in TAUOLA.
//
// Can be invoked by
//   setSeed( 47238, 985439, 0 );
// instead of
// Tauola::setSeed( 47238, 985439, 0 ); 
//
// It can be used with up to date version of library as well
// because of #ifndef rmarin_ condition.

#ifndef rmarin_
// Set seed for TAUOLA-FORTRAN random number generator
extern "C" void rmarin_(int *ijklin, int *ntotin, int *ntot2n);
#endif

/** Set seed for TAUOLA-FORTRAN random number generator */
static void setSeed( int ijklin, int ntotin, int ntot2n ) { rmarin_(&ijklin,&ntotin,&ntot2n); }
