/// \File StvSeedConst.h
/// \author Victor Perev 01/2012
/// \Constants for all seed findersStvSeedConst

#ifndef StvSeedConst_HH
#define StvSeedConst_HH

//#define KNNMAP0 1
#define KNNMAP1 1
//#define KNNMAP2 1

//#define KNNGONE 1


enum ESeeds {kMinHits=5,kMaxHits = 10,kZRange=200};
#define kMinErr  (1e-2)
#define kMaxErr  (3e-1)
#define kRelErr  (1e-3)
#define SEED_ERR(rad) (kMinErr+rad/200*kMaxErr)

typedef float Mtx33F_t[3][3];



#endif
