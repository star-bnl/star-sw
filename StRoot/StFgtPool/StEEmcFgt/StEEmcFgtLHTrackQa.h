/***************************************************************************
 *
 * $Id: StEEmcFgtLHTrackQa.h,v 1.4 2012/04/13 15:08:43 sgliske Exp $
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
 * Revision 1.4  2012/04/13 15:08:43  sgliske
 * updates
 *
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
   virtual Int_t Finish();

   TH1F* getSigHist( Int_t i ){ return (i>=0 && i<4 ? mSig[i] : 0); };
   TH1F* getSigPerHist( Int_t i ){ return (i>=0 && i<4 ? mSigPer[i] : 0); };

   void setFileOutName( const Char_t* name );

 protected:
   StEEmcRawMapMaker *mEEmcRawMapMkr;
   StFgtLHTracking *mFgtLHTkr;
   Float_t mThres;
   std::string mFileOutName;

   TH1F *mSig[4];
   TH1F *mSigPer[4];

 private:   
   ClassDef(StEEmcFgtLHTrackQa,1);

}; 

// inline
inline void StEEmcFgtLHTrackQa::setFileOutName( const Char_t* name ){ mFileOutName = name; };


#endif
