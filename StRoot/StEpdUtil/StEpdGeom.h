#ifndef _StEpdGeom
#define _StEpdGeom

#include "Rtypes.h"
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
  Short_t   mPP;  /// supersector position [1,12]
  Short_t   mTT;  /// tile number on supersector [1,31]
  Short_t   mEW;  /// West/East = +1/-1
  TRandom3* pRan;

  /* these methods are used internally */
  /// z coordinate of the wheel in STAR coordinate system
  /// depends on internal parameter mEW
  Double_t GetZwheel();
  /// phi of the center of the tile in STAR coordinate syste
  /// depends on internal parameters mPP, mTT, mEW
  Double_t GetPhiCenter();
  /// the inner and outer extent of the tile in the radial direction
  /// depends on internal parameters mPP, mTT, mEW
  void     GetRminRmax(Double_t *Rmin, Double_t *Rmax);
  /// the "tile row" of the tile.  Row = [1,16]
  /// depends on internal parameter mTT
  Short_t  Row();
  /// given the uniqueID of a tile (uniqueID = sign*(100*PP+TT) where sign=+1/-1 for West/East wheel
  /// this sets the internal parameters mPP, mTT, and mEW
  void     SetPpTtEw(Short_t uniqueID);

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
  void     GetCorners(Int_t* nCorners, Double_t* x, Double_t* y);
  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  /// depends on internal parameters mPP, mTT, mEW
  Bool_t   IsInTile(Double_t x, Double_t y);

 public:

  StEpdGeom();
  ~StEpdGeom();

  /// center of the tile in STAR coordinate system
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  TVector3 TileCenter(Short_t uniqueID);
  /// center of the tile in STAR coordinate system
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  TVector3 TileCenter(Short_t position, Short_t tilenumber, Short_t eastwest);

  /// returns a random point somewhere on the tile.
  /// assumes a uniform hit density
  /// this is very useful for calculating things like dN/deta
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  TVector3 RandomPointOnTile(Short_t uniqueID);
  /// returns a random point somewhere on the tile.
  /// assumes a uniform hit density
  /// this is very useful for calculating things like dN/deta
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  TVector3 RandomPointOnTile(Short_t position, Short_t tilenumber, Short_t eastwest);

  /// returns the corners of the tile in the plane of the wheel, in STAR coordinate system
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  /// \param *nCorners   this is a RETURNED value. Number of corners the tile has (TT01 has 5, others have 4)
  /// \param x           this is a RETURNED values.  x-coordinates of corners
  /// \param y           this is a RETURNED values.  y-coordinates of corners
  void GetCorners(Short_t uniqueID,
		  Int_t* nCorners, Double_t* x, Double_t* y);
  /// returns the corners of the tile in the plane of the wheel, in STAR coordinate system
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  /// \param *nCorners   this is a RETURNED value. Number of corners the tile has (TT01 has 5, others have 4)
  /// \param x           this is a RETURNED values.  x-coordinates of corners
  /// \param y           this is a RETURNED values.  y-coordinates of corners
  void GetCorners(Short_t position, Short_t tilenumber, Short_t eastwest,
		  Int_t* nCorners, Double_t* x, Double_t* y);


  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  Bool_t IsInTile(Short_t uniqueID,
		  Double_t x, Double_t y);
  /// returns true if (x,y) lies within the tile.  Assumes z=zWheel
  /// useful if the user would like to project a track (using straight line of helix or whatever)
  /// to the plane of the wheel and determine whether it hit a given tile
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  /// \param x    x-coordinate of projected hit
  /// \param y    y-coordinate of projected hit
  Bool_t IsInTile(Short_t position, Short_t tilenumber, Short_t eastwest,
		  Double_t x, Double_t y);

  /// true if this tile is on west side
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  Bool_t IsWest(Short_t uniqueID);
  /// true if this tile is on west side
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  Bool_t IsWest(Short_t position, Short_t tilenumber, Short_t eastwest);

  /// true if this tile is on east side
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  Bool_t IsEast(Short_t uniqueID);
  /// true if this tile is on east side
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  Bool_t IsEast(Short_t position, Short_t tilenumber, Short_t eastwest);

  /// the "tile row" of the tile.  Row = [1,16]
  /// \param uniqueID    identifier of the tile = sign*(100*PP+TT) where sign=+/- for West/East
  Short_t Row(Short_t uniqueID);
  /// the "tile row" of the tile.  Row = [1,16]
  /// \param position   position of supersector [1,12]
  /// \param tilenumber tile on supsersector [1,31]
  /// \eastwest         east (-1) or west (+1) wheel
  Short_t Row(Short_t position, Short_t tilenumber, Short_t eastwest);

  ClassDef(StEpdGeom,1)


};


#endif
