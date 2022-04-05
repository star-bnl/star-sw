/*!
 * \class  StKinkLocalTrack
 * \brief  Auxiliary class for the kink finder
 * \author Camelia Mironov, KSU
 * \date   Jan,2004
 *
 *
 */
#ifndef StKinkLocalTrack_hh
#define StKinkLocalTrack_hh

#include "TObject.h"
#include "StThreeVectorD.hh"
class StTrack;

class StKinkLocalTrack:public TObject {
public:
  StKinkLocalTrack();
  StKinkLocalTrack(StTrack* trk);


  // StKinkLocalTrack(const StKinkLocalTrack&);                  use default
  // const StKinkLocalTrack& operator=(const StKinkLocalTrack&); use default
  
  Int_t  Compare(const TObject *obj) const;
  Bool_t IsSortable() const { return 1; }
  Bool_t IsEqual(const TObject *obj) const;
 
  Float_t endRadius2D() const;
  Float_t startRadius2D() const;
  StTrack* trackBack()const;

protected:
  
  StThreeVectorD mStartPoint;  
  StThreeVectorD mLastPoint;  
  Float_t mEndRadius2D;
  Float_t mStartRadius2D;
  StTrack* mTrack;
  
private:
ClassDef(StKinkLocalTrack,0)
};

inline Float_t StKinkLocalTrack::endRadius2D() const { return mEndRadius2D; }
inline Float_t StKinkLocalTrack::startRadius2D() const { return mStartRadius2D; }
inline StTrack* StKinkLocalTrack::trackBack()const {return mTrack;}

#endif
