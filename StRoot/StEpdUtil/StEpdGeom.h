#ifndef _StEpdGeom
#define _StEpdGeom

//#include "Rtypes.h"
#include "TVector3.h"

/*************************************
 * \author Mike Lisa
 * \date 4 Jan 2018
 *
 * \description:
 *  class defining geometrical aspects of an EPD Tile
 *  (position of center, RandomPointOnTile(), etc.)
 *
 * The user may pass the PP/TT/EW _or_ the uniqueID to
 *   most functions.  No option to pass StEpdHit object,
 *   because we want to avoid StObject-dependence.
 *   Instead, simply use StEpdHit::id(),
 *   e.g. RandomPointOnTile(hit->id())
 *************************************/

class TRandom3;

class StEpdGeom{
 private:
  short   mPP;  /// supersector position [1,12]
  short   mTT;  /// tile number on supersector [1,31]
  short   mEW;  /// West/East = +1/-1
  TRandom3* pRan;

  double mPhiCenter[12][31][2];  // PP,TT,EW
  double mRmin[16];   // row
  double mRmax[16];   // row
  double mRave[16];   // row

  // EastNumberOfOverlappingBbcTiles[PP-1][TT-1] =  number of BBC tiles that overlap
  static short mEastNumberOfOverlappingBbcTiles[12][9];  
  // EastBbcTilesWhichOverlap[PP-1][TT-1][j] gives the BBC tile ID of the jth overlapping BBC tile
  static short mEastBbcTilesWhichOverlap[12][9][3];  // 

  // WestNumberOfOverlappingBbcTiles[PP-1][TT-1] =  number of BBC tiles that overlap
  static short mWestNumberOfOverlappingBbcTiles[12][9];  
  // WestBbcTilesWhichOverlap[PP-1][TT-1][j] gives the BBC tile ID of the jth overlapping BBC tile
  static short mWestBbcTilesWhichOverlap[12][9][3];  // 


  void InitializeGeometry();

  /* these methods are used internally */
  /// z coordinate of the wheel in STAR coordinate system
  /// depends on internal parameter mEW
  double GetZwheel();

  /// phi of the center of the tile in STAR coordinate syste
  /// depends on internal parameters mPP, mTT, mEW
  //--------- obsolete (always the plan; that's why it was private)  double GetPhiCenter();
  /// the inner and outer extent of the tile in the radial direction
  /// depends on internal parameters mPP, mTT, mEW
  //--------- obsolete (always the plan; that's why it was private)  void     GetRminRmax(double *Rmin, double *Rmax);
  /// the "tile row" of the tile.  Row = [1,16]
  /// depends on internal parameter mTT
  short  Row();

  /// given the uniqueID of a tile (uniqueID = sign*(100*PP+TT) where sign=+1/-1 for West/East wheel
  /// this sets the internal parameters mPP, mTT, and mEW
  void     SetPpTtEw(short uniqueID);

  /// center of the tile in STAR coordinate system
  /// depends on internal parameters mPP, mTT, mEW
  TVector3 TileCenter();

  /// returns a random point somewhere on the tile.
  /// assumes a uniform hit density
  /// this is very useful for calculating things like dN/deta
  /// depends on internal parameters mPP, mTT, mEW
  TVector3 RandomPointOnTile();

  /// returns the corners of the tile in the plane of the wheel, in STAR coordinate system
  /// \param *nCorners   this is a RETURNED value. Number of corners the tile has (TT01 has 5, others have 4)
  /// \param x           this is a RETURNED values.  x-coordinates of corners
  /// \param y           this is a RETURNED values.  y-coordinates of corners
  /// depends on internal parameters mPP, mTT, mEW
  void     GetCorners(int* nCorners, double* x, double* y);

  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  /// depends on internal parameters mPP, mTT, mEW
  bool   IsInTile(double x, double y);

  /// returns a list of (the IDs of) BBC tiles that overlap with a given EPD tile
  /// \param nOverlappingBbcTiles         *output* parameter: number of BBC tiles that overlaps this EPD tile (even just barely)
  /// \param BbcTileIDs                   *output* parameter: array of BBC tile IDs
  void GetOverlappingBbcTiles(int* nOverlappingBbcTiles, short* BbcTileIDs);





 public:

  StEpdGeom();
  ~StEpdGeom();

  unsigned short position(short uniqueID);
  unsigned short tile(short uniqueID);


  /// center of the tile in STAR coordinate system
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  TVector3 TileCenter(short uniqueID);

  /// center of the tile in STAR coordinate system
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  TVector3 TileCenter(short position, short tilenumber, short eastwest);

  /// returns a random point somewhere on the tile.
  /// assumes a uniform hit density
  /// this is very useful for calculating things like dN/deta
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  TVector3 RandomPointOnTile(short uniqueID);

  /// returns a random point somewhere on the tile.
  /// assumes a uniform hit density
  /// this is very useful for calculating things like dN/deta
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  TVector3 RandomPointOnTile(short position, short tilenumber, short eastwest);

  /// returns the corners of the tile in the plane of the wheel, in STAR coordinate system
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  /// \param *nCorners   this is a RETURNED value. Number of corners the tile has (TT01 has 5, others have 4)
  /// \param x           this is a RETURNED values.  x-coordinates of corners
  /// \param y           this is a RETURNED values.  y-coordinates of corners
  void GetCorners(short uniqueID,
		  int* nCorners, double* x, double* y);
  /// returns the corners of the tile in the plane of the wheel, in STAR coordinate system
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  /// \param *nCorners   this is a RETURNED value. Number of corners the tile has (TT01 has 5, others have 4)
  /// \param x           this is a RETURNED values.  x-coordinates of corners
  /// \param y           this is a RETURNED values.  y-coordinates of corners
  void GetCorners(short position, short tilenumber, short eastwest,
		  int* nCorners, double* x, double* y);

  /// returns a list of (the IDs of) BBC tiles that overlap with a given EPD tile
  /// \param uniqueID                     identifier of the EPD tile = sign*(100*PP+TT) where sign=+/- for West/East
  /// \param nOverlappingBbcTiles         *output* parameter: number of BBC tiles that overlaps this EPD tile (even just barely)
  /// \param BbcTileIDs                   *output* parameter: array of BBC tile IDs
  void GetOverlappingBbcTiles(short uniqueID,
			      int* nOverlappingBbcTiles, short* BbcTileIDs);

  /// returns a list of (the IDs of) BBC tiles that overlap with a given EPD tile
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  /// \param nOverlappingBbcTiles         *output* parameter: number of BBC tiles that overlaps this EPD tile (even just barely)
  /// \param BbcTileIDs                   *output* parameter: array of BBC tile IDs
  void GetOverlappingBbcTiles(short position, short tilenumber, short eastwest,
			      int* nOverlappingBbcTiles, short* BbcTileIDs);



  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  bool IsInTile(short uniqueID,
		  double x, double y);

  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  bool IsInTile(short position, short tilenumber, short eastwest,
		  double x, double y);

  /// true if this tile is on west side
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  bool IsWest(short uniqueID);

  /// true if this tile is on west side
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  bool IsWest(short position, short tilenumber, short eastwest);

  /// true if this tile is on east side
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  bool IsEast(short uniqueID);
  /// true if this tile is on east side
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  bool IsEast(short position, short tilenumber, short eastwest);

  /// the "tile row" of the tile.  Row = [1,16]
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  short Row(short uniqueID);

  /// the "tile row" of the tile.  Row = [1,16]
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  short Row(short position, short tilenumber, short eastwest);

};

inline bool StEpdGeom::IsWest(short uniqueID){return uniqueID>0;}
inline bool StEpdGeom::IsWest(short position, short tilenumber, short eastwest){return eastwest>0;}
inline bool StEpdGeom::IsEast(short uniqueID){return uniqueID<0;}
inline bool StEpdGeom::IsEast(short position, short tilenumber, short eastwest){return eastwest<0;}
inline unsigned short StEpdGeom::position(short uniqueID){return abs(uniqueID/100);}
inline unsigned short StEpdGeom::tile(short uniqueID){return abs(uniqueID%100);}




#endif
