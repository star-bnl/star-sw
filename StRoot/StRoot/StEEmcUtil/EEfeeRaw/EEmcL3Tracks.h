/* Hey Emacs this is -*-c++-*- */
#ifndef EEmcL3Tracks_h
#define EEmcL3Tracks_h
/*********************************************************************
 * $Id: EEmcL3Tracks.h,v 1.11 2003/09/11 19:41:02 zolnie Exp $
 *********************************************************************
 * $Log: EEmcL3Tracks.h,v $
 * Revision 1.11  2003/09/11 19:41:02  zolnie
 * updates for gcc3.2
 *
 * Revision 1.10  2003/09/02 17:57:55  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.9  2003/06/02 17:34:35  zolnie
 * fixed bug in EEmcHelix
 *
 * Revision 1.8  2003/05/31 02:31:30  zolnie
 * bring the changes back
 *
 * Revision 1.6  2003/05/30 21:07:52  zolnie
 * added track length and number of points
 *
 * Revision 1.5  2003/05/30 20:37:09  zolnie
 * added L3 track flag to EEmcHelix
 *
 * Revision 1.4  2003/05/28 21:02:49  zolnie
 * added getNumberOfTracks method
 *
 * Revision 1.3  2003/05/27 19:11:44  zolnie
 * added dE/dx info
 *
 * Revision 1.2  2003/05/26 14:44:34  zolnie
 * rewritten implementation of EEmcL3Tracks using TClonesArray
 * introduced a common Makefile and mklinkdef.pl
 *
 * Revision 1.1  2003/05/20 19:22:59  zolnie
 * new additions for ..... :)
 *
 *********************************************************************/
#include <cmath>
#include <ctime>

#ifndef MAXFLOAT
#define MAXFLOAT 3E38
#endif


#include "TObject.h"
#include "TClonesArray.h"

class EEmcHelix : public TObject {
public:
  EEmcHelix() { mOx=mOy=mOz=mPx=mPy=mPz=mLength=mB=0.0; mQ=mPoints=mFlag=0; };
  EEmcHelix(Float_t x, Float_t y, Float_t z, Float_t px, Float_t py, Float_t pz,
	    Int_t   q, Float_t B, Int_t  np, Float_t l , Int_t mFlag);
  EEmcHelix(const EEmcHelix &h);

  void  setOrigin  (Float_t x ,Float_t y ,Float_t z ) { mOx=x; mOy=y; mOz=z; }
  void  setMomentum(Float_t px,Float_t py,Float_t pz) { mPx=px;mPy=py;mPz=pz;}
  void  setQ       (Int_t   q )                       { mQ = q;  }
  void  setB       (Float_t B )                       { mB = B;  } 
  void  setFlag    (Int_t   f )                       { mFlag   = f; }
  void  setPoints  (Int_t   n )                       { mPoints = n; }
  void  setLength  (Float_t l )                       { mLength = l; }

  void  getOrigin  (Float_t &x,Float_t &y,Float_t &z) const {x=mOx;y=mOy;z=mOz;}
  void  getMomentum(Float_t &x,Float_t &y,Float_t &z) const {x=mPx;y=mPy;z=mPz;}

  Float_t Ox()     const { return mOx;  }
  Float_t Oy()     const { return mOy;  }
  Float_t Oz()     const { return mOz;  }
  Float_t Px()     const { return mPx;  }
  Float_t Py()     const { return mPy;  }
  Float_t Pt()     const { return ::sqrt(mPx*mPx+mPy*mPy);  }
  Float_t Pz()     const { return mPz;  }
  Int_t   Q ()     const { return mQ;   }
  Float_t B ()     const { return mB;   } 
  Int_t   Points() const { return mPoints;}
  Float_t Length() const { return mLength;}
  Int_t   Flag()   const { return mFlag;}

  void    print(FILE *fd=stdout) const;
  
private:
  Float_t mOx;     // origin x
  Float_t mOy;     // origin y
  Float_t mOz;     // origin z
  Float_t mPx;     // x momentum at origin
  Float_t mPy;     // y momentum at origin
  Float_t mPz;     // z momentum at origin
  Float_t mB;      // field
  Int_t   mQ;      // charge
  Int_t   mPoints; //
  Float_t mLength; //
  Int_t   mFlag;   // 0==primary, 1==secondary
  

  ClassDef(EEmcHelix,4)   
};

class EEmcL3Tracks : public TObject {
public:
  EEmcL3Tracks();
  virtual ~EEmcL3Tracks();
  int     add  (EEmcHelix &h, Float_t dedx);
  void    clear();
  void    setVertex(float  x,float  y,float  z) {mVertX=x; mVertY=y; mVertZ=z;};

  void    getVertex(float& x,float& y,float& z) {x=mVertX; y=mVertY; z=mVertZ;};
  Float_t getdEdx  (int i)    {return((0<=i && i<mNTracks)?mDedx[i]:-MAXFLOAT);}
  Int_t   getNumberOfTracks() {return mNTracks;}
  EEmcHelix* getHelix (int i) ; 
  
  void       print(FILE *f = stdout) const;

private:
  static const Int_t mAllocTracks;//!
  Int_t              mTrackSize;//!
  Int_t              mNTracks;  //
  Float_t            mVertX;    //
  Float_t            mVertY;    //
  Float_t            mVertZ;    //
  TClonesArray      *mHelix;    //-> array with helices
  Float_t           *mDedx;     //[mNTracks]
  ClassDef(EEmcL3Tracks,2) 
};
#endif


