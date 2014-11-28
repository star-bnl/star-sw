/***************************************************************************
 * $Id: FTPV1P0.Banks.cxx,v 1.2 2007/12/24 06:04:12 fine Exp $
 * Author: M.W. Shculz, Jeff Landgraf, M.J. LeVine, H.Klay, H.Huemmler
 ***************************************************************************
 * Description: Overrides (mostly for the swap functions, 
 *              print functions of each bank)
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0.Banks.cxx,v $
 * Revision 1.2  2007/12/24 06:04:12  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.1  2000/01/18 18:01:19  levine
 * Hummler's implementaiton of FTPC reader. Note that method
 *
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,
 * 				   Sequence **SeqData)
 *
 * causes exit() since the required #include file has not yet been
 * (correctly) implemented.
 *
 *
 *
 **************************************************************************/
#include <assert.h>

#include "StDaqLib/GENERIC/swaps.hh"
#include "FTPV1P0.Banks.hh"
using namespace OLDEVP;

/*--------------FTP IMPLEMENTATION-------------------*/

int classname(Bank_FTPRBP)::swap()
{
  // get bank length 
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  // returns zero means none needed
  if(!iret) return(0);  
  // returns negative means error
  if(iret < 0) return iret;

  // swap body first  (only sectors get swapped)
  iret = swap_raw(header.ByteOrder,(INT32 *)Sector,sizeof(Sector)/4);
  assert(iret > 0);

  // swap the header
  iret = header.swap();

  return iret;
}
  
int classname(Bank_FTPADCD)::swap()
{
  return header.swap();   // nothing to swap in the data
}

int classname(Bank_FTPADCR)::swap()
{
  return header.swap();
}

int classname(Bank_FTPPADK)::swap()
{
  // Get length
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  if(!iret) return 0;
  if(iret < 0) return iret;

  // Swap body
  iret = swap_raw(header.ByteOrder,&bpADC,6);
  assert(iret>0);
  
  // Swap header
  return header.swap();
}

int classname(Bank_FTPCPPR)::swap()
{
  // get number of words
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  if(!iret) return 0;
  if(iret<0) return iret;

  // swap body 
  iret = swap_short(header.ByteOrder,(INT32 *)entry,10240);
  assert(iret>0);

  // swap header
  return header.swap();
}

int classname(Bank_FTPSEQD)::swap()
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
  
int classname(Bank_FTPCFGR)::swap()
{
  return header.swap();
}

int classname(Bank_FTPBADR)::swap()
{
  return header.swap();
}

int classname(Bank_FTPPEDR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;

  return header.swap();
}

int classname(Bank_FTPRMSR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;
  
  return header.swap();
}

int classname(Bank_FTPGAINR)::swap()
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
  for(int i = 0; i < FTP_MZPADS; i++)
  {
    char temp=curr[0];
    curr[0] = curr[1];
    curr[1] = temp;
    curr += 4;
  }
  return iret;
}
