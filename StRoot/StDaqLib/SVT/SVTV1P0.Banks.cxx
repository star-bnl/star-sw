/***************************************************************************
 *
 * $Id: SVTV1P0.Banks.cxx,v 1.1 2000/06/06 18:08:31 jml Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: Overrides (mostly for the swap functions, 
 *              print functions of each bank)
 *
 ***************************************************************************
 *
 * $Log: SVTV1P0.Banks.cxx,v $
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *
 **************************************************************************/
#include <assert.h>
#include <iostream.h>

#include "StDaqLib/GENERIC/swaps.hh"
#include "SVTV1P0.Banks.hh"

int classname(Bank_SVTRBP)::swap()
{
  // get bank length 
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  // returns zero means none needed
  if(!iret) return(0);  
  // returns negative means error
  if(iret < 0) return iret;

  // swap body first  (only mz get swapped)
  iret = swap_raw(header.ByteOrder,(INT32 *)Mz,sizeof(Mz)/4);
  assert(iret > 0);

  // swap the header
  iret = header.swap();

  return iret;
}
  
int classname(Bank_SVTADCD)::swap()
{
  return header.swap();   // nothing to swap in the data
}

int classname(Bank_SVTADCR)::swap()
{
  return header.swap();
}

int classname(Bank_SVTANODK)::swap()
{
  // Get length
  int words = header.BankLength;

  int iret = swap_raw(header.ByteOrder,&words,1);

  //  if(!iret) return 0;

#ifdef sparc
  if(!iret) {
    return 0;
  }
  else if(iret < 0) 
    return iret;
  else
    iret = swap_raw(header.ByteOrder,&bpADC,12);
#elif i386
  if(!iret) 
    return 0;
  else if(iret < 0) 
    return iret;
  else
    iret = swap_raw(header.ByteOrder,&bpADC,12);
#else
#error "Unknown machine type"
#endif

  //  if(iret < 0) return iret;

  // Swap body
  //  iret = swap_raw(header.ByteOrder,&bpADC,nWordsToSwap);
  assert(iret>0);
  
  // Swap header
  return header.swap();
}

int classname(Bank_SVTCPPR)::swap()
{
  // get number of words
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  if(!iret) return 0;
  if(iret<0) return iret;

  // swap body 
  iret = swap_short(header.ByteOrder,(INT32 *)entry,12288);
  assert(iret>0);

  // swap header
  return header.swap();
}

int classname(Bank_SVTSEQD)::swap()
{
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);
  
  if(!iret) return 0;
  if(iret < 0) return iret;
  
  words -= (sizeof(header)/4);
  swap_short(header.ByteOrder,(INT32 *)sequence,words);
  assert(iret>0);

  iret = header.swap();
  return iret;
}
  
int classname(Bank_SVTCFGR)::swap()
{
  return header.swap();
}

int classname(Bank_SVTBADR)::swap()
{
  return header.swap();
}

int classname(Bank_SVTPEDR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;

  return header.swap();
}

int classname(Bank_SVTRMSR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;
  
  return header.swap();
}

int classname(Bank_SVTGAINR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,2);
  if(iret <= 0) return iret;
  
  // if no swapping neccessary, already returned
  // if iret not 1 then can't untangle shorts
  if(iret != 1) return -1;

  // we know we want to swap the first two bytes of every GAINR entry
  // but we can't use swap_short, which really swaps words of shorts

  // get the header swapping out of the way first
  iret = header.swap();
  assert(iret == 1);

  char *curr = (char *)Gain;
  for(int i = 0; i < SVT_MZANODES; i++)
  {
    char temp=curr[0];
    curr[0] = curr[1];
    curr[1] = temp;
    curr += 4;
  }
  return iret;
}
 
  
int classname(Bank_SVTMZCLD)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumHybrids,1);
  if(iret <= 0) return iret;

  int nsp = 0;
  int *word = &NumHybrids;
  word++; //bump to first instance of PadRow
  for (int i=0; i<NumHybrids; i++){
    swap_raw(header.ByteOrder,word,2);
    nsp = word[1];
    word+=2; //point to first cluster
    swap_short(header.ByteOrder,word,2*nsp);
    word+=2*nsp; //bump past clusters
  }

  return header.swap();
}
  



