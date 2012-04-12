/***************************************************************************
 *
 * $Id: StEEmcFgtLHTrackQa.h,v 1.3 2012/04/12 17:12:05 sgliske Exp $
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
 * Revision 1.3  2012/04/12 17:12:05  sgliske
 * update to not use A2EMaker but StEEmcRawMaker
 *
 * Revision 1.2  2012/04/11 22:13:24  sgliske
 * update
 *
 * Revision 1.1  2012/04/11 21:39:19  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _StEEmcFgtLHTrackQa_H_
#define _StEEmcFgtLHTrackQa_H_

#include "StMaker.h"
class StEEmcRawMapMaker;
class StFgtLHTracking;

class StEEmcFgtLHTrackQa : public StMaker {
 public:
   // constructors
   StEEmcFgtLHTrackQa( const Char_t* name = "fgtTracking", const Char_t* rawMapMkrName = "EEmcRawMapMaker", const Char_t* fgtLHTkrName = "FgtLHTracker" );

   // deconstructor
   virtual ~StEEmcFgtLHTrackQa();

   // default equals operator and copy constructor OK

   virtual Int_t Make();
   virtual Int_t Init();

   TH1F* getEnergy(){ return mEnergy; };
   TH1F* getEnergyPerTrack(){ return mEnergyPerTrack; };

 protected:
   StEEmcRawMapMaker *mEEmcRawMapMkr;
   StFgtLHTracking *mFgtLHTkr;

   TH1F *mEnergyPerTrack;
   TH1F *mEnergy;

 private:   
   ClassDef(StEEmcFgtLHTrackQa,1);

}; 

#endif
