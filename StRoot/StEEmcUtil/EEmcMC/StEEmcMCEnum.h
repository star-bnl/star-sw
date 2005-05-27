#ifndef STAR_StEEmcMCEnum
#define STAR_StEEmcMCEnum

enum EEmcVolId {
  // for Tower
  kEEmcTowerHalfId =  100000,
  kEEmcTowerPhiId  =    1000,
  kEEmcTowerEtaId  =      10,
  kEEmcTowerDepId  =       1,

  // for SMDs
  kEEmcSmdHalfId   = 1000000,
  kEEmcSmdPhiId    =   10000,
  kEEmcSmdPlaneId  =    1000,
  kEEmcSmdStripId  =        1
};

enum MCDepth {
  kUnknownDepth    = 0,
  kPreShower1Depth = 1,
  kPreShower2Depth = 2,
  kTower1Depth     = 3,
  kTower2Depth     = 4,
  kPostShowerDepth = 5
};

enum MCDetectorId {
  kEEmcMCUnknownId    = 0,
  kEEmcMCTowerId      = 1,
  kEEmcMCPreShower1Id = 2,
  kEEmcMCPreShower2Id = 3,
  kEEmcMCSmdUStripId  = 4,
  kEEmcMCSmdVStripId  = 5,
  kEEmcMCPostShowerId = 6
};

#endif
