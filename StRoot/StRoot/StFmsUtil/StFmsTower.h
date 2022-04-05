// $Id: StFmsTower.h,v 1.2 2015/10/21 15:58:05 akio Exp $
//
// $Log: StFmsTower.h,v $
// Revision 1.2  2015/10/21 15:58:05  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsTower.h
 \brief     Declaration of StFmsTower, a simple FMS tower wrapper
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#ifndef STROOT_STFMSPOINTMAKER_STFMSTOWER_H_
#define STROOT_STFMSPOINTMAKER_STFMSTOWER_H_

#include "TObject.h"

class StFmsHit;
class StFmsDbMaker;

namespace FMSCluster {  // $NMSPC
/**
 Lightweight wrapper around an StFmsHit for use in tower clustering.

 Clustering requires identifying tower neighbors, which is most easily done
 via row and column number. As these aren't stored in StFmsHit, we store an
 StFmsHit pointer here with its row and column number. This removes the need
 to recalculate row and column (which requires database access) each time they
 are needed. We also store the index of the cluster (if any) that the tower
 becomes associated with during clustering.

 The StFmsTower does not own the StFmsHit; it merely references it. Therefore it
 is vital that the StFmsHit have a longer lifetime than the StFmsTower i.e.
 do not clear your StFmsCollection until after you have finished clustering!

 Inherits from TObject so it can be stored in a ROOT container.
 */
class StFmsTower : public TObject {
 public:
  /** Default constructor. */
  StFmsTower();
  /**
   Constructor.

   Initialize with an StFmsHit, which the StFmsTower does *now* own. It should
   therefore have longer lifetime that the StFmsTower.
   */
  explicit StFmsTower(StFmsHit* fmsHit);
  // Use default copy constructor and assignment operator
  /** Destructor. */
  ~StFmsTower();
  /**
   Initialize tower row and column information from the database.

   Returns true upon successful initialization, false if something goes wrong.
   <b>Important</b>: an uninitialized tower should NOT be used!
   */
  Bool_t initialize(StFmsDbMaker* database);
  /** Returns true, as StFmsTower can be sorted in a ROOT container. */
  Bool_t IsSortable() const { return kTRUE; }
  /**
   Test if another StFmsTower is a neighbor of this tower.

   A neighbor is the tower immediately above, below, left or right of this one
   i.e. NOT diagonally adjacent towers.

   \verbatim
   i.e. _
      _|_|_
     |_|_|_|
       |_|
   \endverbatim
   */
  Bool_t isNeighbor(const StFmsTower& tower) const;
  /**
   Returns the hit information for this tower.

   (nullptr if unknown, in which case you probably shouln't use this tower!)
   */
  const StFmsHit* hit() const { return mHit; }
  /** Returns the column of this tower (-1 if unknown). */
  Int_t column() const { return mColumn; }
  /** Returns the row of this tower (-1 if unknown). */
  Int_t row() const { return mRow; }
  /** Returns the cluster index of this tower (-1 if unassociated). */
  Int_t cluster() const { return mCluster; }
  /** Sets the cluster index and returns the new index. */
  void setCluster(Int_t cluster) { mCluster = cluster; }
  /** Set & get local xy */
  void setXY(double x, double y) {mX=x; mY=y;}
  double x() const;
  double y() const;
  double e() const;
  double w() const;


 protected:
  const StFmsHit* mHit;  //!< Hit information, not owned by StFmsTower
  Int_t mColumn;  /// < Column number, starts at 1, horizontal (STAR x-coord)
  Int_t mRow;     /// < Row number, starts at 1, vertical (STAR y-coord)
  Int_t mCluster; /// < Index of cluster the tower is associated with
  Double_t mX;    /// local x [cm]
  Double_t mY;    /// local y [cm]
  Double_t mE;    /// Energy
  Double_t mW;    /// Tower width [cm]
  ClassDef(StFmsTower, 0)
};

inline double StFmsTower::x() const {return mX;}
inline double StFmsTower::y() const {return mY;}
inline double StFmsTower::e() const {return mE;}
inline double StFmsTower::w() const {return mW;}

}  // namespace FMSCluster
#endif  // STROOT_STFMSPOINTMAKER_STFMSTOWER_H_
