#ifndef ST_FAST_JET_AREA_PARS_H
#define ST_FAST_JET_AREA_PARS_H

class StFastJetAreaPars : public TObject{
public:

  static const int active_area;
  static const int active_area_explicit_ghosts;
  static const int one_ghost_passive_area;
  static const int passive_area;

  StFastJetAreaPars()
    : mArea_type(active_area)
    , mGhost_maxrap(1.5)
    , mRepeat(1)
    , mGhost_area(0.04)
    , mGrid_scatter(1.0)
    , mPt_scatter(0.1)
    , mMean_ghost_pt(1e-100)
  {
  }
  ~StFastJetAreaPars(){}

  int    areaType       () const { return mArea_type; }
  double ghostMaxRap            () const { return mGhost_maxrap;       }
  int    repeat() const { return mRepeat; }
  double ghostArea           () const { return mGhost_area;     }
  double gridScatter              () const { return mGrid_scatter;        }
  double ptScatter() const { return mPt_scatter; }
  double meanGhostPt() const { return mMean_ghost_pt; }

  void setAreaType (int area_type) { mArea_type = area_type; }
  void setGhostMaxRap (double ghost_maxrap) { mGhost_maxrap = ghost_maxrap; }
  void setRepeat (int repeat) { mRepeat = repeat; }
  void setGhostArea (double ghost_area) { mGhost_area = ghost_area; }
  void setGridScatter (double grid_scatter) { mGrid_scatter = grid_scatter; }
  void setPtScatter(double pt_scatter) { mPt_scatter = pt_scatter; }
  void setMeanGhostPt (double mean_ghost_pt) { mMean_ghost_pt = mean_ghost_pt; }
  
private:
  int mArea_type;
  double mGhost_maxrap;
  int mRepeat;
  double    mGhost_area;
  double    mGrid_scatter;
  double mPt_scatter;
  double mMean_ghost_pt;

  ClassDef(StFastJetAreaPars,0)
};

#endif	// ST_FAST_JET_PARS_H
