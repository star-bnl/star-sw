/* Hey Emacs this is -*-c++-*- */
#ifndef EEmcL3Tracks_h
#define EEmcL3Tracks_h
/*********************************************************************
 * $Id: EEmcL3Tracks.h,v 1.1 2003/05/20 19:22:59 zolnie Exp $
 *********************************************************************
 * $Log: EEmcL3Tracks.h,v $
 * Revision 1.1  2003/05/20 19:22:59  zolnie
 * new additions for ..... :)
 *
 *********************************************************************/
#include <time.h>


#include "TObject.h"


class EEmcHelix     : public TObject {
public:
  Float_t ox,oy,oz; // origin
  Float_t px,py,pz; // momentum
  Float_t B;        // field
  Int_t   q;        // charge
  
  EEmcHelix() { ox=oy=oz=px=py=pz=0.0; q=0; B=0.0; };
  EEmcHelix(EEmcHelix &h) { 
    ox=h.ox; oy=h.oy; oz=h.oz;
    px=h.px; py=h.py; pz=h.pz;
    q =h.q;  B =h.B;
  };
  ClassDef(EEmcHelix,1)

};

class EEmcL3Tracks : public TObject {
public:
  EEmcL3Tracks();
  virtual ~EEmcL3Tracks();
  int  add(EEmcHelix &h, Float_t dedx);
  void clear();
  
  void setVertex(float  x,float  y,float  z) {mVertX=x; mVertY=y; mVertZ=z;};
  void getVertex(float& x,float& y,float& z) {x=mVertX; y=mVertY; z=mVertZ;};

  void print(FILE *f = stdout) const;

private:
  static const int   mAllocTracks;//!
  int                mTrackSize;//!
  int                mNTracks;  //
  Float_t            mVertX;    //
  Float_t            mVertY;    //
  Float_t            mVertZ;    //
  EEmcHelix         *mHelix;    //[mNTracks];
  Float_t           *mDedx;     //[mNTracks];

  ClassDef(EEmcL3Tracks,1) 

};
#endif


