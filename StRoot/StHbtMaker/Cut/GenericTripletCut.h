/***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a do-nothing triplet cut that simply says "true" to every triplet
 *
 ***************************************************************************/


#ifndef GenericTripletCut_hh
#define GenericTripletCut_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtTripletCut.h"

class GenericTripletCut : public StHbtTripletCut{
public:
  GenericTripletCut();
  //~GenericTripletCut();

  virtual bool Pass(const StHbtTriplet*);

  virtual StHbtString Report();


private:
  long mNTripletsPassed;
  long mNTripletsFailed;

  ClassDef(GenericTripletCut, 1)

};

#endif
