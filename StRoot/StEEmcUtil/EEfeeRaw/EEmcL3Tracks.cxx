// $Id: EEmcL3Tracks.cxx,v 1.10 2003/09/11 19:41:02 zolnie Exp $ 
// $Log: EEmcL3Tracks.cxx,v $
// Revision 1.10  2003/09/11 19:41:02  zolnie
// updates for gcc3.2
//
// Revision 1.9  2003/09/05 15:04:21  zolnie
// remove Stiostream/iostream from the source code
//
// Revision 1.8  2003/09/02 17:57:55  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.7  2003/06/02 17:34:35  zolnie
// fixed bug in EEmcHelix
//
// Revision 1.6  2003/05/31 02:31:30  zolnie
// bring the changes back
//
// Revision 1.4  2003/05/30 21:07:52  zolnie
// added track length and number of points
//
// Revision 1.3  2003/05/27 19:11:44  zolnie
// added dE/dx info
//
// Revision 1.2  2003/05/26 14:44:34  zolnie
// rewritten implementation of EEmcL3Tracks using TClonesArray
// introduced a common Makefile and mklinkdef.pl
//
// Revision 1.1  2003/05/20 19:22:59  zolnie
// new additions for ..... :)
//

//#include <Stiostream.h>
#include <cmath>
#include "EEmcL3Tracks.h"

ClassImp(EEmcHelix)

EEmcHelix::EEmcHelix(Float_t x , Float_t y , Float_t z, 
		     Float_t px, Float_t py, Float_t pz,
		     Int_t   q , Float_t B , 
		     Int_t   np, Float_t l , Int_t flag)
{
  mOx=x;  mOy=y;  mOz=z; 
  mPx=px; mPy=py; mPz=pz;
  mQ      = q;
  mB      = B;
  mPoints = np;
  mLength = l;
  mFlag   = flag;
}


EEmcHelix::EEmcHelix(const EEmcHelix &h) 
{
  h.getOrigin  (mOx,mOy,mOz);
  h.getMomentum(mPx,mPy,mPz);
  mQ      = h.Q();
  mB      = h.B();
  mPoints = h.Points();
  mLength = h.Length();
  mFlag   = h.Flag();
};


void 
EEmcHelix :: print(FILE *fd) const
{
  fprintf(fd,"\torigin  : %+f %+f %+f\n",mOx,mOy,mOz);
  fprintf(fd,"\tmomentum: %+f %+f %+f\n",mPx,mPy,mPz);
  fprintf(fd,"\tcharge  : %+1d\tfield : %+f\n",mQ,mB);
  fprintf(fd,"\tpoints  : %2d \tlength: %+f\n",mPoints,mLength);
  fprintf(fd,"\tflag    : %2d (0x%04x)\n",mFlag,mFlag);
}



ClassImp(EEmcL3Tracks)


const Int_t  EEmcL3Tracks::mAllocTracks=32;

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks::EEmcL3Tracks() {
  mHelix   = new TClonesArray("EEmcHelix",1000);
  mDedx    = new Float_t[mAllocTracks];
  mTrackSize = mAllocTracks;
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks::~EEmcL3Tracks() 
{ 
  if(mHelix) delete    mHelix;
  if(mDedx)  delete [] mDedx;
}



int
EEmcL3Tracks::add(EEmcHelix &h, Float_t de)
{
  // add dEdx
  if(mNTracks >= mTrackSize ) { 
    Int_t    newSize = mTrackSize + mAllocTracks;
    Float_t *newDedx = new Float_t[newSize];
    memcpy(newDedx,mDedx,mTrackSize*sizeof(Float_t));
    delete [] mDedx;
    mDedx      = newDedx;
    mTrackSize = newSize;
    //cerr << "more " << mTrackSize << endl;
  }

  TClonesArray &helices = *mHelix; 
  mDedx[mNTracks]  = de;
  EEmcHelix *helix = new(helices[mNTracks]) EEmcHelix(h); 
  mNTracks++;
  //if(helix) helix->print();
  return ( (helix==NULL) ? 1 : 0 );
}


//--------------------------------------------------
//
//--------------------------------------------------
void 
EEmcL3Tracks::clear() 
{ 
  mHelix->Clear();
  mNTracks=0;
  mVertX  = mVertY = mVertZ = MAXFLOAT;
}



EEmcHelix* 
EEmcL3Tracks::getHelix (int i) 
{ 
  return (0<=i && i<mNTracks) ?  ((EEmcHelix *)(mHelix->At(i))) : NULL;
}


  


//--------------------------------------------------
//
//--------------------------------------------------
void EEmcL3Tracks :: print(FILE *fd) const
{
  fprintf(fd,"L3 tracks %d:\n",mNTracks);
  if(mNTracks<=0) return;
  fprintf(fd,"\tvertex: (%+8.3f %+8.3f %+8.3f)\n",mVertX,mVertY,mVertZ);
  for(Int_t i=0;i<mHelix->GetEntries();i++) {
    fprintf(fd,"\ttrack#%d dE/dx=%g\n",i,mDedx[i]);
    ((EEmcHelix *)(mHelix->At(i)))->print(fd);
  }
}



