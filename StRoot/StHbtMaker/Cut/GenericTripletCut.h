/***************************************************************************
 *
 * $Id: GenericTripletCut.h,v 1.4 2001/06/03 21:04:56 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *   A do-nothing triplet cut that simply says "true" to every triplet.
 *
 ***************************************************************************
 *
 * $Log: GenericTripletCut.h,v $
 * Revision 1.4  2001/06/03 21:04:56  willson
 * Cuts on entrance separation
 *
 * Revision 1.3  2000/08/08 23:39:31  laue
 * Updated for standalone version
 *
 * Revision 1.2  2000/04/12 01:53:35  willson
 * Initial Installation - Comments Added
 *
 *
 ***************************************************************************/

#ifndef GenericTripletCut_hh
#define GenericTripletCut_hh

#include "StHbtMaker/Base/StHbtTripletCut.h"

class GenericTripletCut : public StHbtTripletCut{
public:
  GenericTripletCut(float EntSepCut = 0.0);
  //~GenericTripletCut();

  virtual bool Pass(const StHbtTriplet*);

  virtual StHbtString Report();


private:
  long mNTripletsPassed;
  long mNTripletsFailed;
  float mEntSepCut;

#ifdef __ROOT__
  ClassDef(GenericTripletCut, 1)
#endif

};

#endif
