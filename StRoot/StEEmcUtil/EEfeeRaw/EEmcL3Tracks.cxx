// $Id: EEmcL3Tracks.cxx,v 1.2 2003/05/26 14:44:34 zolnie Exp $ 
// $Log: EEmcL3Tracks.cxx,v $
// Revision 1.2  2003/05/26 14:44:34  zolnie
// rewritten implementation of EEmcL3Tracks using TClonesArray
// introduced a common Makefile and mklinkdef.pl
//
// Revision 1.1  2003/05/20 19:22:59  zolnie
// new additions for ..... :)
//

#include <iostream.h>
#include <math.h>
#include "EEmcL3Tracks.h"



ClassImp(EEmcHelix)


EEmcHelix::EEmcHelix(Float_t x ,Float_t y ,Float_t z, Float_t px,Float_t py,Float_t pz,
		     Int_t   q ,Float_t B)
{
  mOx=x;  mOy=y;  mOz=z; 
  mPx=px; mPy=py; mPz=pz;
  mQ    = q;
  mB    = B;
}


EEmcHelix::EEmcHelix(const EEmcHelix &h) 
{
  h.getOrigin  (mOx,mOy,mOz);
  h.getMomentum(mPx,mPy,mPz);
  mQ    = h.Q();
  mB    = h.B();
};


void 
EEmcHelix :: print(FILE *fd) const
{
  fprintf(fd,"\torigin  : %+f %+f %+f\n",mOx,mOy,mOz);
  fprintf(fd,"\tmomentum: %+f %+f %+f\n",mPx,mPy,mPz);
  fprintf(fd,"\tcharge  : %+1d\n",mQ);
  fprintf(fd,"\tfield   : %+f\n",mB);
}



ClassImp(EEmcL3Tracks)

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks ::  EEmcL3Tracks() {
  mHelix   = new TClonesArray("EEmcHelix",1000);
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks ::  ~EEmcL3Tracks() 
{ 
  if(mHelix) delete mHelix;
}



int
EEmcL3Tracks::add(EEmcHelix &h, Float_t de)
{
  TClonesArray &helices = *mHelix;
  EEmcHelix *helix = new(helices[mNTracks++]) EEmcHelix(h); 

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
#ifdef MAXFLOAT 
  mVertX  = mVertY = mVertZ = MAXFLOAT;
#else
  mVertX  = mVertY = mVertZ = 3E38;
#endif
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
    fprintf(fd,"\ttrack#%d\n",i);
    ((EEmcHelix *)(mHelix->At(i)))->print(fd);
  }
}



