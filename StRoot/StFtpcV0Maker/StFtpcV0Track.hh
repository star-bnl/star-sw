///////////////////////////////////////////////////////
//  This is the track class.
//
// Writen by Mike Heffner 10/30/98
//  Changed to StFtpcV0Track by Janet Seyboth 10/23/00
///////////////////////////////////////////////////////

#ifndef ST_FTPC_V0_TRACK
#define ST_FTPC_V0_TRACK

#include <Stiostream.h>

#include "StPhysicalHelix.hh"
#include "StThreeVector.hh"
#include "StLorentzVector.hh"
#include "StGlobals.hh"
#include "V0PhysicalConstants.hh"

class StFtpcV0Track: public StPhysicalHelix
{
 protected:
  StDouble mB;

 public:
  StFtpcV0Track(StThreeVector<double> momentum,
	      StThreeVector<double> origin,
	      double magField, double charge);

  StFtpcV0Track();
 ~StFtpcV0Track();

  double GetMagField() const {return mB;}

  void display() const;

};


#endif  //ST_FTPC_V0_TRACK
