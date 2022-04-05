/***************************************************************************
 * $Id: TPCV1P0.Banks.cxx,v 1.3 2007/12/24 06:04:31 fine Exp $
 * Author: M.W. Shculz, Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description: Overrides (mostly for the swap functions, 
 *              print functions of each bank)
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: TPCV1P0.Banks.cxx,v $
 * Revision 1.3  2007/12/24 06:04:31  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.2  1999/07/02 04:43:22  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/
#include <assert.h>

#include "StDaqLib/GENERIC/swaps.hh"
#include "TPCV1P0.Banks.hh"

using namespace OLDEVP;

int classname(Bank_TPCRBP)::swap()
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
  
int classname(Bank_TPCADCD)::swap()
{
  return header.swap();   // nothing to swap in the data
}

int classname(Bank_TPCADCR)::swap()
{
  return header.swap();
}

int classname(Bank_TPCPADK)::swap()
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

int classname(Bank_TPCCPPR)::swap()
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

int classname(Bank_TPCSEQD)::swap()
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
  
int classname(Bank_TPCCFGR)::swap()
{
  return header.swap();
}

int classname(Bank_TPCBADR)::swap()
{
  return header.swap();
}

int classname(Bank_TPCPEDR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;

  return header.swap();
}

int classname(Bank_TPCRMSR)::swap()
{
  int iret = swap_raw(header.ByteOrder,&NumEvents,1);
  if(iret <= 0) return iret;
  
  return header.swap();
}

int classname(Bank_TPCGAINR)::swap()
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
  for(int i = 0; i < TPC_MZPADS; i++)
  {
    char temp=curr[0];
    curr[0] = curr[1];
    curr[1] = temp;
    curr += 4;
  }
  return iret;
}
 
  
  



