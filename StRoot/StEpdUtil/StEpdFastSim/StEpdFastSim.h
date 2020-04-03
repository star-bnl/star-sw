#ifndef _StEpdFastSim
#define _StEpdFastSim

class TClonesArray;
class TRandom3;
class StEpdGeom;
class TVector3;
class StPicoEpdHit;

class StEpdFastSim{
 private:
  double mWID;       // WID of Landau distribution (assumed MPV=1)
  StEpdGeom* mGeom;
  TRandom3* mRan;
  //  StPicoEpdHit* mHits[2][12][31];   // easy access.  These get nonzero only when a hit is created.  Keeping tracks of hits that are made, internally.
  StPicoEpdHit* mHitsEast[1232];   // easy access.  These get nonzero only when a hit is created.  Keeping tracks of hits that are made, internally.  Index is abs(UniqueID)
  StPicoEpdHit* mHitsWest[1232];   // easy access.  These get nonzero only when a hit is created.  Keeping tracks of hits that are made, internally.  Index is abs(UniqueID)

  // quick lookups
  double RingRadii[17];             // 17 rather than 16, because it is EDGES  (17 edges of 16 rows)

  short FindStruckTile(TVector3 HitPosition);

  TClonesArray* mTheHits;

 public:
  StEpdFastSim(double WID=0.2);
  ~StEpdFastSim();
  void SetWid(double WID);
  TClonesArray* GetPicoHits(TClonesArray* momenta, TVector3 PrimVertex);
};

inline void StEpdFastSim::SetWid(double WID){mWID=WID;}

#endif
