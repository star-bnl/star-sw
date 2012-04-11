/***************************************************************************
 *
 * $Id: StEEmcFgtLHTrackQa.h,v 1.1 2012/04/11 21:39:19 sgliske Exp $
 * Author: S. Gliske, April 2012
 *
 ***************************************************************************
 *
 * Description: Plot energy response in the EEMC for FGT tracks from
 * the LHTracking class.
 *
 ***************************************************************************
 *
 * $Log: StEEmcFgtLHTrackQa.h,v $
 * Revision 1.1  2012/04/11 21:39:19  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _StEEmcFgtLHTrackQa_H_
#define _StEEmcFgtLHTrackQa_H_

#include "StMaker.h"
class StEEmcA2EMaker;

class StEEmcFgtLHTrackQa : public StMaker {
 public:
   // constructors
   StEEmcFgtLHTrackQa( const Char_t* name = "fgtTracking", const Char_t* a2eMkrName = "EEmcA2EMaker" );

   // deconstructor
   virtual ~StEEmcFgtLHTrackQa();

   // default equals operator and copy constructor OK

   virtual Int_t Make();
   virtual Int_t Init();

   TH1F* getEnergy(){ return mEnergy; };
   TH1F* getEnergyPerTrack(){ return mEnergyPerTrack; };

 protected:
   StEEmcA2EMaker *mEEmcA2EMkr;
   TH1F *mEnergyPerTrack;
   TH1F *mEnergy;

 private:   
   ClassDef(StEEmcFgtLHTrackQa,1);

}; 

#endif
