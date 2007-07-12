//
//  StL3RareTrack.h
//

#ifndef STL3RARETRACK_HH
#define STL3RARETRACK_HH

#include "Rtypes.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "TMath.h"
class StL3EventSummary;
class StGlobalTrack;


class StL3RareTrack : public TObject
{
 public:
      StL3RareTrack();
      StL3RareTrack(StGlobalTrack* track);
      ~StL3RareTrack(){};
      int   tracknumber() const;
      int   flag() const;
      float px() const;
      float py() const;
      float pz() const;
      float p() const;
      short chargesign() const;
      float rapidity(float mass) const;
      float pt() const;
      float dca2d() const;
      float dedx() const;
      short ndedx() const;
      float chisqxy() const;
      float chisqz() const;
      short npntpossible() const;
      short npntfit() const;
      float dedxExpected(float mass, float charge = 1) const;
      float dedxPi() const;
      float dedxProton() const;
      float dedxDeuteron() const;
      float dedxTriton() const;
      float dedxHe3() const;
      float dedxHe4() const;
      void  SetTrigType(int type);
      int   trigtype()  const; //33 = random track

 private:
      int   mTracknumber;
      int   mFlag;
      float mPx;
      float mPy;
      float mPz;
      short mChargesign;
      float mDca2d;
      float mDedx;
      short mNDedx;
      float mChisqXY;
      float mChisqSZ;
      short mNPntfit;
      short mNPntpossible;
      int   mTrigType;

  ClassDef(StL3RareTrack,2)
};
 
inline int   StL3RareTrack::tracknumber() const {return mTracknumber;}
inline int   StL3RareTrack::flag() const {return mFlag;}
inline float StL3RareTrack::px() const {return mPx;}
inline float StL3RareTrack::py() const {return mPy;}
inline float StL3RareTrack::pz() const {return mPz;}
inline float StL3RareTrack::p() const {return ::sqrt(mPx*mPx+mPy*mPy+mPz*mPz);}
inline float StL3RareTrack::pt() const {return ::sqrt(mPx*mPx+mPy*mPy);}
inline short StL3RareTrack::chargesign() const {return mChargesign;}
inline float StL3RareTrack::dca2d() const {return mDca2d;} 
inline float StL3RareTrack::dedx() const {return mDedx;}
inline short StL3RareTrack::ndedx() const {return mNDedx;}
inline float StL3RareTrack::chisqxy() const {return mChisqXY;} 
inline float StL3RareTrack::chisqz() const {return mChisqSZ;} 
inline short StL3RareTrack::npntfit() const {return mNPntfit;}
inline short StL3RareTrack::npntpossible() const {return mNPntpossible;}
inline float StL3RareTrack::dedxPi() const {return dedxExpected(0.139,1);} 
inline float StL3RareTrack::dedxProton() const {return dedxExpected(0.939,1);} 
inline float StL3RareTrack::dedxDeuteron() const {return dedxExpected(1.88,1);} 
inline float StL3RareTrack::dedxTriton() const {return dedxExpected(2.82,1);} 
inline float StL3RareTrack::dedxHe3() const {return dedxExpected(2.82,2);} 
inline float StL3RareTrack::dedxHe4() const {return dedxExpected(3.76,2);} 
inline int   StL3RareTrack::trigtype() const {return mTrigType;}

#endif
