#ifndef __StarPDGEncodings_hh__
#define __StarPDGEncodings_hh__
/// Helper function to define PDG ids for heavy ions                                                                                                                                                       
/// @param z Charge of the heavy ion                                                                                                                                                                       
/// @param a Atomic number of the heavy ion                                                                                                                                                                
/// @param l Number of lambdas in a hypernucleus                                                                                                                                                           
int hid( int z, int a, int l=0 );

enum {   
  kTriton      = 1000000000,
  kHyperTriton = 1000000001,
  kAntiHyperTriton = 1000000002,
  kDalitz      = 1000000111,
  kLambda1520  =   20003122
};

#endif
