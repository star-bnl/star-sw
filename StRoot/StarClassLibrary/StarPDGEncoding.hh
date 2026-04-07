#ifndef __StarPDGEncodings_hh__
#define __StarPDGEncodings_hh__
/// Helper function to define PDG ids for heavy ions                                                                                                                                                       
/// @param z Charge of the heavy ion                                                                                                                                                                       
/// @param a Atomic number of the heavy ion                                                                                                                                                                
/// @param l Number of lambdas in a hypernucleus                                                                                                                                                           
int hid( int z, int a, int l=0 );

enum {   
  kTriton          = 1000000000,
  kHyperTriton     = 1000000001,
  kAntiHyperTriton = 1000000002,
  kDalitz          = 1000000111,
  kLambda1520      =   20003122,

  /// STAR-specific PDG codes for d-hyperon pseudoparticles (loosely bound
  /// deuteron-hyperon states). No official PDG numbers exist for these.
  kDLambda         = 1000000010, ///< d + Lambda  (GID 60100)
  kDSigma0         = 1000000011, ///< d + Sigma0  (GID 60101)
  kDXiMinus        = 1000000012, ///< d + Xi-     (GID 60102)
  kDXiZero         = 1000000013  ///< d + Xi0     (GID 60103)
};

#endif
