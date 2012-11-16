/// \File StvSeedConst.h
/// \author Victor Perev 01/2012
/// \Constants for all seed findersStvSeedConst

#ifndef StvSeedConst_HH
#define StvSeedConst_HH

enum ESeeds {kMinHits=5,kMaxHits = 10};
#define kMinErr  (1e-1)
#define kMaxErr  (3e-1)
#define SEED_ERR(rad) (kMinErr + rad/200*kMaxErr)
#endif
