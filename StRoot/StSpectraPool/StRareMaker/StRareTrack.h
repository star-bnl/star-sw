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
  int tracknumber() const;
  int iflag() const;
  float px() const;
  float py() const;
  float pz() const;
  float p() const;
  int chargesign() const;
  float rapidity(float mass) const;
  float pt() const;
  float dca() const;
  float dedx() const;
  int   ndedx() const;
  float chisqxy() const;
  float chisqz() const;
  int   npntpossible() const;
  int   npntfit() const;
  float dedxExpected(float mass, float charge = 1) const;
  float dedxPi() const;
  float dedxProton() const;
  float dedxDeuteron() const;
  float dedxTriton() const;
  float dedxHe3() const;
  float dedxHe4() const;
  int trigtype()  const; //33 = random track
  void SetTrigType(int type);

 private:
  int   ftracknumber;
  int   fiflag;
  float fpx;
  float fpy;
  float fpz;
  int   fchargesign;
  float fdca;
  float fdedx;
  int   fndedx;
  float fchisqxy;
  float fchisqz;
  int   fnpntpossible;
  int   fnpntfit;
  float fdedxPion;
  float fdedxProton;
  float fdedxDeuteron;
  float fdedxTriton;
  float fdedxHe3;
  float fdedxHe4;
  int ftrigtype;

  ClassDef(StRareTrack,1)
};
 
inline int   StRareTrack::tracknumber() const {return ftracknumber;}
inline int   StRareTrack::iflag() const {return fiflag;}
inline float StRareTrack::px() const {return fpx;}
inline float StRareTrack::py() const {return fpy;}
inline float StRareTrack::pz() const {return fpz;}
inline float StRareTrack::p() const {return sqrt(fpx*fpx+fpy*fpy+fpz*fpz);}
inline float StRareTrack::pt() const {return sqrt(fpx*fpx+fpy*fpy);}
inline int   StRareTrack::chargesign() const {return fchargesign;}
inline float StRareTrack::dca() const {return fdca;} 
inline float StRareTrack::dedx() const {return fdedx;}
inline int   StRareTrack::ndedx() const {return fndedx;}
inline float StRareTrack::chisqxy() const {return fchisqxy;} 
inline float StRareTrack::chisqz() const {return fchisqz;} 
inline int   StRareTrack::npntpossible() const {return fnpntpossible;}
inline int   StRareTrack::npntfit() const {return fnpntfit;}
inline float StRareTrack::dedxPi() const {return dedxExpected(0.139,1);} 
inline float StRareTrack::dedxProton() const {return dedxExpected(0.939,1);} 
inline float StRareTrack::dedxDeuteron() const {return dedxExpected(1.88,1);} 
inline float StRareTrack::dedxTriton() const {return dedxExpected(2.82,1);} 
inline float StRareTrack::dedxHe3() const {return dedxExpected(2.82,2);} 
inline float StRareTrack::dedxHe4() const {return dedxExpected(3.76,2);} 
inline int   StRareTrack::trigtype() const {return ftrigtype;}

#endif
