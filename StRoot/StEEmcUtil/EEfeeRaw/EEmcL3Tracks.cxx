// $Id: EEmcL3Tracks.cxx,v 1.1 2003/05/20 19:22:59 zolnie Exp $ 
// $Log: EEmcL3Tracks.cxx,v $
// Revision 1.1  2003/05/20 19:22:59  zolnie
// new additions for ..... :)
//

#include <iostream.h>
#include "EEmcL3Tracks.h"


ClassImp(EEmcL3Tracks)



const int EEmcL3Tracks::mAllocTracks=32;

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks ::  EEmcL3Tracks() {
  mTrackSize=mAllocTracks;
  mHelix = new EEmcHelix[mTrackSize];
  mDedx  = new Float_t  [mTrackSize];
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEmcL3Tracks ::  ~EEmcL3Tracks() { }



int
EEmcL3Tracks::add(EEmcHelix &h, Float_t dedx)
{
  if(mNTracks>=mTrackSize) { 

    int newTrackSize    = mTrackSize + mAllocTracks ;
    Float_t   *newDedx  = new Float_t   [newTrackSize];    
    EEmcHelix *newHelix = new EEmcHelix [newTrackSize];    

    memcpy(newDedx ,mDedx ,sizeof(Float_t  )*mTrackSize);
    for(int i=0; i<mTrackSize; i++) newHelix[i]=mHelix[i];

    delete [] mHelix;
    delete [] mDedx;
    mHelix = newHelix;
    mDedx  = newDedx;
    mTrackSize = newTrackSize;
  }

  mHelix[mNTracks] = h;
  mDedx [mNTracks] = dedx;

  mNTracks++;
  return 0;
}


//--------------------------------------------------
//
//--------------------------------------------------
void 
EEmcL3Tracks::clear() 
{
  mNTracks=0;
  mVertX = mVertY = mVertZ = 0.0;
}


//--------------------------------------------------
//
//--------------------------------------------------
void EEmcL3Tracks :: print(FILE *fd) const
{
  EEmcHelix *h=mHelix;
  fprintf(fd,"L3 tracks:\n");
  fprintf(fd,"\tvertex: (%+8.3f %+8.3f %+8.3f)\n",mVertX,mVertY,mVertZ);
  for(int i=0; i<mNTracks; h++,i++) {
    fprintf(fd,"\ttrack%02d: dEdx=%+8.3e q=%1d B=%+8.3e\n",
	i,mDedx[i], h->q,h->B);
    fprintf(fd,"\torigin   : %+8.3f %+8.3f %+8.3f\n",h->ox,h->oy,h->oz);
    fprintf(fd,"\tp0       : %+8.3f %+8.3f %+8.3f\n",h->px,h->py,h->pz);
  }

  //printf("mComment=%s\n",mComment);
}



