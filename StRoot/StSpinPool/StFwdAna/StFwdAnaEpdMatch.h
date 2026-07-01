/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpose of this class is to match the photon candidates to the EPD hits and properly set the variables of the matched photons (#FcsPhotonCandidate). Also contains static functions to generate EPD adjacency tile maps and polylines

  DESCRIPTION
  Loops over all EPD hits and fills the static array #mAllEpdNmip which can then be used for further analysis outside this class. Loops over all the #FcsPhotonCandidate in #StMuFcsAnaData::mPhArr and for each #FcsPhotonCandidate projects the #FcsPhotonCandidate onto the EPD plane; for each projected position it loops over all EPD tiles and checks if this position lies inside any EPD tile; if yes set nMIP to zero and store the tiles usual EPD key (100*pp+tt) into the #FcsPhotonCandidate's data. It then sets the nMIP value for the #FcsPhotonCandidate to the matching intersecting EPD tile. It also grabs the intersecting tiles nmip value (nmiptile) and the 8 adjacent tiles' nmip values and determines a sum (nmipsum) and a maximum out of all 9 (nmipmax) and stores those in #FcsPhotonCandidate. The finding of the matched and adjacent tiles is done in the static function #CheckInsideEpdTile(). It is static so that it can be re-used in other #StMuFcsAna analysis modules.

  LOG
  @[January 22, 2026] > Copied static methods of finding adjacenct EPD tiles from #StMuEpdRun22QaMaker
  @[January 14, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker
  @[June 8, 2026] > Implemented #mAllEpdNmip array to hold the nmip value for each tile and supersector so calling #CheckInsideEpdTile() should happen after this filling. Moved EPD hit loop outside photon candidate loop since nmip can be grabbed from #mAllEpdNmip. You can now grab the nmip of an epd tile from outside this class using the static #epdNmip(). Improved the #CheckInsideEpdTile() algorithm by copying over the nmip tile, nmip adjacency sum, and nmip adjacency max filling algorithm from #StMuFcsAnaEpdMatchQa (which no longer has its own 'CheckInsideEpdTile()'). Moved some of the QA histograms and drawing to the new #StMuFcsAnaEpdMatchQa. Modified #EpdCCWOuterCorner() to grab polyline from internal static map or make one if it doesn't exist.
  @[June 11, 2026] > Fixed a bug where #EpdTilePoly() was doing an extra loop for printout but not actually printing so commented out the loop
  @[June 17, 2026] > Fixed a bug in #CheckInsideEpdTile() for when there was only 1 match but was not getting into the ncorner loop to set the best match since ncorner=1 and was being skipped over. Wrote new EPD match algorithm that will find which region of the EPD tile a projected FCS cluster/point falls into. The algorithm converts all x,y positions to r,phi coordinates since the EPD tiles are rectangular in these coordinates and then it takes the difference in the r values and phi values to determine which EPD adjacency the point lies in. In this coordinate space the calculation is set up such that a postive difference in r means outer; a negative difference in r means inner; a postiive difference in phi means counter-clockWise (CCW) and negative differnce in phi means clockwise (CW). If both are satisified it can go outer CCW, inner CCW, etc. Once it finds this adjacency it encodes it in #FcsPhotonCandidate::mEpdFoundRegion. Once it finds the region it then checks those adjacencies and records the maximum nmip into #FcsPhotonCandidate::mEpdMatch[0] and #FcsPhotonCandidate::mEpdHitNmip[0]; where the 1 to 4 indices will contain the matched tile and the adjacenct tiles in an outer CCW convention. The r,phi region is defined by the geometry of the EPD tiles; it is half the size of the tile
  @[July 1, 2026] Changed name from StMuFcsAnaEpdMatch to StFwdAnaEpdMatch
*/


#ifndef STFWDANA_STFWDANAEPDMATCH_HH
#define STFWDANA_STFWDANAEPDMATCH_HH

#include <map>

#include "StFwdAnaVirtual.h"

class StFwdAnaEpdMatch : public StFwdAnaVirtual
{
public:
  StFwdAnaEpdMatch();
  ~StFwdAnaEpdMatch();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StFwdAnaData* data);
  virtual Int_t DoMake(StFwdAnaData* anadata);

  static void CheckInsideEpdTile(StEpdGeom* epdgeo, FcsPhotonCandidate* photon, Double_t projx, Double_t projy);   ///< Main algorithm that will check if a #FcsPhotonCandidate intersects any EPD tile or any OuterCCW EPD tiles if no EPD tile is found. Once such a match is found it will appropriately fill the nmip values in #FcsPhotonCandidate so that another analysis module can use this informaiton as a selection criteria
  
  static std::vector<Int_t> GetAdjacentEpdIds(Int_t pp,Int_t tt);
  static void GetEpdPPandTTFromId(Int_t id, Int_t& pp, Int_t& tt);

  /* Only checked for West EPD
     Chosen so that #TPolyLine can be made to encompass a larger area
     Adjacency functions as a sqaure (CW=ClockWise,CCW=CounterClockWise)
     |----------------------------|
     | outerCCW | outer | outerCW |
     |----------|-------|---------|
     |   CCW    |  main |   CW    |
     |----------|-------|---------|
     | innerCCW | inner | innerCW |
     |----------------------------|
     
     
     X (Beam line)
  */
  static void GetEpdTileOuter(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);       ///< Get the tile going away from beam line from the given tile
  static void GetEpdTileOuterCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);    ///< Get the tile going away and counterclockwise (increasing pp)
  static void GetEpdTileCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);         ///< Get the tile going counterclockwise (increasing pp)
  static void GetEpdTileInnerCCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);    ///< Get the tile going in and counterclockwise (increasing pp)
  static void GetEpdTileInner(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);       ///< Get the tile going towards the beam line (in) from the given tile
  static void GetEpdTileInnerCW(Int_t pp, Int_t tt, Int_t &newpp, Int_t& newtt);     ///< Get the tile in and clockwise (decreasing pp)
  static void GetEpdTileCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);          ///< Get the tile going clockwise (decreasing pp)
  static void GetEpdTileOuterCW(Int_t pp, Int_t tt, Int_t& newpp, Int_t& newtt);     ///< Get the tile going away and clockwise (decreasing pp)
  
  //void GetAdjacentEpdTile(int pp, int tt, int& pp_adj, int& tt_adj) const;
  static TPolyLine* EpdTilePoly(StEpdGeom* epdgeo, short pp, short tt);        ///< Return a new polyline using corners from StEpdGeom
  static TPolyLine* EpdCCWOuterCorner(StEpdGeom* epdgeo, short pp, short tt);  ///< Return a new polyline using adjacency of outer CCW. This will either be a newly created one or an existing one from #mEpdCcwLines. Therefore it will be deleted by this class
  static TPolyLine* EpdCWOuterCorner(StEpdGeom* epdgeo, short pp, short tt);   ///< Return a new polyline using adjacency of outer CW
  static TPolyLine* EpdCWInnerCorner(StEpdGeom* epdgeo, short pp, short tt);   ///< Return a new polyline using adjacency of inner CW
  static TPolyLine* EpdCCWInnerCorner(StEpdGeom* epdgeo, short pp, short tt);  ///< Return a new polyline using adjacency of inner CCW

  static float epdNmip(short pp, short tt){ return mAllEpdNmip[pp-1][tt-1]; }    ///< Return the nmip value for a given EPD supersector and tile. Correctly accounts for shift in supersctor (pp) and tile (tt) so user should pass in the "normal" EPD values. See #mAllEpdNmip

  void PaintEpdProjections(TCanvas* canv, const char* savename)   const;         ///< Paints the histograms in this class
  
protected:
  TH1* mH2F_EpdProjHitMap = 0;          ///< Distribution of x,y projections of photon candidates onto STAR EPD plane in x,y space
  TH1* mH2F_EpdProjHitMap_Vcut = 0;     ///< Distribution of x,y projections of photon candidates onto STAR EPD plane in x,y space with cut |vertex|<150cm
  TH1* mH2F_EpdNmip = 0;                ///< Nmip distributions for EPD matched projected clusters (x-axis bin 1) and points (y-axis bin 2)
  
  static std::map<Int_t,TPolyLine*> mEpdCcwLines;  ///< Map of EPD key to TPolyline to keep track of created polyline objects

  static float mAllEpdNmip[12][31];     ///< Array that will hold the nmip values for all west EPD tiles in an event. West EPD has 12 supersectors and 31 tiles per supersector. EPD database and map starts counting from 1 so need to downshift by 1 when using values supersector and tile numbers from EPD geometry. If using #epdNmip() this is handled correctly
  
  ClassDef(StFwdAnaEpdMatch,1)
};

#endif

