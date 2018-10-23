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
 *
 * Because some users will use the sign of the tileId
 *  to denote east/west, and some will use an explicit
 *  integer, we provide overloaded functions, to allow both.
 *
 ******************************************/



class StBbcGeom{
 private:

  TVector3 mEastTileCenter[18];
  TVector3 mWestTileCenter[18];
  double mX[6];   // x-coordinates of hexagonal tile corners, if tile center is at (0,0)
  double mY[6];   // y-coordinates of hexagonal tile corners, if tile center is at (0,0)
  unsigned short mPMT[18];  // phototube number corresponding to a given inner BBC tile.  Same for east and west BBCs

  unsigned short mNtilesOfPmt[16];          // how many tiles correspond to the phototube?  (either 1 or 2)
  unsigned short mTileNumbersOfPmt[16][2];  // WHICH tiles correspond to the phototube?


// mEastNumberOfOverlappingEpdTiles[BB-1]  gives the number of EPD tiles that have any overlap with BBC tile BB
  static short mEastNumberOfOverlappingEpdTiles[18];
// mEastEpdTilesWhichOverlap[BB-1][i]  gives the EPD tile id of the jth EPD tile that overlaps BBC inner tile BB
  static short mEastEpdTilesWhichOverlap[18][10];

// mWestNumberOfOverlappingEpdTiles[BB-1]  gives the number of EPD tiles that have any overlap with BBC tile BB
  static short mWestNumberOfOverlappingEpdTiles[18];
// mWestEpdTilesWhichOverlap[BB-1][i]  gives the EPD tile id of the jth EPD tile that overlaps BBC inner tile BB
  static short mWestEpdTilesWhichOverlap[18][10];





  void InitializeGeometry();

 public:
  StBbcGeom();
  ~StBbcGeom();

  //------------------------------------------------------

  /// tells the position of the tile center in STAR Coordinates
  /// \param tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  TVector3 TileCenter(short tileId);

  /// tells the position of the tile center in STAR Coordinates
  /// \param absValueTileNumber         this is a positive number between 1 and 16, inclusive
  /// \param eastwest                   <=0  for East, >0 for West
  TVector3 TileCenter(unsigned short absValueTileNumber, short eastwest);

  //------------------------------------------------------

  /// returns the 6 corners of the hexagonal tile in STAR Coordinates
  /// \param tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  /// \param x,y       pointers to arrays of six corners
  void GetCorners(short tileId, double* x, double* y);

  /// returns the 6 corners of the hexagonal tile in STAR Coordinates
  /// \param absValueTileNumber         this is a positive number between 1 and 16, inclusive
  /// \param eastwest                   <=0  for East, >0 for West
  /// \param x,y                       pointers to arrays of six corners
  void GetCorners(unsigned short absValueTileNumber, short eastwest, double* x, double* y);

  //------------------------------------------------------

  /// returns a list of (the IDs of) EPD tiles that overlap with a given BBC tile
  /// \param tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  /// \param nOverlappingEpdTiles       *output* parameter.  Number of EPD tiles that overlaps this BBC tile (0, 8, or 10)
  /// \param EpdTileIDs                 *output* parameter: list of EPD Tile IDs
  void GetOverlappingEpdTiles(short tileId,
			      int* nOverlappingEpdTiles, short* EpdTileIDs);


  /// returns a list of (the IDs of) EPD tiles that overlap with a given BBC tile
  /// \param absValueTileNumber         this is a positive number between 1 and 16, inclusive
  /// \param eastwest                   <=0  for East, >0 for West
  /// \param nOverlappingEpdTiles       *output* parameter.  Number of EPD tiles that overlaps this BBC tile (0, 8, or 10)
  /// \param EpdTileIDs                 *output* parameter: list of EPD Tile IDs
  void GetOverlappingEpdTiles(unsigned short absValueTileNumber, short eastwest,
			      int* nOverlappingEpdTiles, short* EpdTileIDs);

  //------------------------------------------------------

  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param  tileId    |tileId| = 1..18 (inner BBC).  Sign of tileId tells side.  +/- for West/East
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  bool IsInTile(short tileId, double x, double y);

  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param absValueTileNumber         this is a positive number between 1 and 16, inclusive
  /// \param eastwest                   <=0  for East, >0 for West
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  bool IsInTile(unsigned short absValueTileNumber, short eastwest, double x, double y);


  /// returns the phototube number (beginning at one) corresponding to the tile number (beginning at one)
  /// it is the same on the East and West sides
  /// \param tileNumber     inner tile number, [1,18] inclusive
  unsigned short PmtOfTile(unsigned short tileNumber);

  /// returns the NUMBER OF TILES and the tile numbers corresponding to a given phototube
  /// the BBC is such a pain in the neck this way!!
  /// most PMTs correspond to one tile only.
  ///   But PMT #7 corresponds to tiles 7 and 9, and PMT #12 corresponds to tiles 13 and 15
  /// At least the correspondence is the same on both sides
  /// \param pmtNumber     phototube number, from 1 to 16, inclusive
  /// \param nTiles        output parameter, either 1 or 2.  tells the number of tiles corresponding to the PMT
  /// \param tileNumbers   array of tile numbers, between 1 and 18, corresponding to the PMT
  void GetTilesOfPmt(unsigned short pmtNumber, unsigned short* nTiles, unsigned short* tileNumbers);

};


inline unsigned short StBbcGeom::PmtOfTile(unsigned short tile){return mPMT[tile-1];}

#endif
