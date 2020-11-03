#ifndef STRARETRACK_HH
#define STRARETRACK_HH
#include "TObject.h"
#include <math.h>

class StPrimaryTrack;

class StRareTrack : public TObject{

 public:
  StRareTrack();
  StRareTrack(StPrimaryTrack* track);
  ~StRareTrack(){};
  int   tracknumber() const;
  int   iflag() const;
  float px() const;
  float py() const;
  float pz() const;
  float p() const;
  short chargesign() const;
  float rapidity(float mass) const;
  float pt() const;
  float dca() const;
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
  int   trigtype()  const; //33 = random track
  void  SetTrigType(int type);

 private:
  int   ftracknumber;
  int   fiflag;
  float fpx;
  float fpy;
  float fpz;
  short fchargesign;
  float fdca;
  float fdedx;
  short fndedx;
  float fchisqxy;
  float fchisqz;
  short fnpntpossible;
  short fnpntfit;
  int   ftrigtype;

  ClassDef(StRareTrack,2)
};
 
inline int   StRareTrack::tracknumber() const {return ftracknumber;}
inline int   StRareTrack::iflag() const {return fiflag;}
inline float StRareTrack::px() const {return fpx;}
inline float StRareTrack::py() const {return fpy;}
inline float StRareTrack::pz() const {return fpz;}
inline float StRareTrack::p() const {return ::sqrt(fpx*fpx+fpy*fpy+fpz*fpz);}
inline float StRareTrack::pt() const {return ::sqrt(fpx*fpx+fpy*fpy);}
inline short StRareTrack::chargesign() const {return fchargesign;}
inline float StRareTrack::dca() const {return fdca;} 
inline float StRareTrack::dedx() const {return fdedx;}
inline short StRareTrack::ndedx() const {return fndedx;}
inline float StRareTrack::chisqxy() const {return fchisqxy;} 
inline float StRareTrack::chisqz() const {return fchisqz;} 
inline short StRareTrack::npntpossible() const {return fnpntpossible;}
inline short StRareTrack::npntfit() const {return fnpntfit;}
inline float StRareTrack::dedxPi() const {return dedxExpected(0.139,1);} 
inline float StRareTrack::dedxProton() const {return dedxExpected(0.939,1);} 
inline float StRareTrack::dedxDeuteron() const {return dedxExpected(1.88,1);} 
inline float StRareTrack::dedxTriton() const {return dedxExpected(2.82,1);} 
inline float StRareTrack::dedxHe3() const {return dedxExpected(2.82,2);} 
inline float StRareTrack::dedxHe4() const {return dedxExpected(3.76,2);} 
inline int   StRareTrack::trigtype() const {return ftrigtype;}

#endif
