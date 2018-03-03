#ifndef _StBbcGeom
#define _StBbcGeom

//#include "Rtypes.h"
#include "TVector3.h"

/*************************************
 * \author Mike Lisa
 * \date 1 Mar 2018
 *
 * \description:
 *  class defining geometrical aspects of the inner BBC
 *  (position of tile center, tile corners, etc.)
 *
 * The BBC is a holy nightmare because one PMT connects
 *  to *either* one or two disjoint tiles.  Also, the
 *  PMT number is the same as the tile number sometimes,
 *  but not all the time.  Plus the numbering starts
 *  at zero sometimes and not other times.
 *
 * In this class, we are only talking about tiles
 *  and tile geometry.  Another class will give you the
 *  map from PMT number to tile(s).
 *
 ******************************************/



class StBbcGeom{
 private:

  TVector3 mEastTileCenter[18];
  TVector3 mWestTileCenter[18];
  double mX[6];   // x-coordinates of hexagonal tile corners, if tile corner is at (0,0)
  double mY[6];   // y-coordinates of hexagonal tile corners, if tile corner is at (0,0)
  void InitializeGeometry();

 public:
  StBbcGeom();
  ~StBbcGeom();

  /// tells the position of the tile center in STAR Coordinates
  /// \param tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  TVector3 TileCenter(short tileId);

  /// returns the 6 corners of the hexagonal tile in STAR Coordinates
  /// \param tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  /// \param x,y       pointers to arrays of six corners
  void GetCorners(short tileId, double* x, double* y);


  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param  tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  bool IsInTile(short tileId, double x, double y);


  ClassDef(StBbcGeom, 0)
};

#endif
