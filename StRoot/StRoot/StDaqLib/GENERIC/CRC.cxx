/***************************************************************************
 * $Id: CRC.cxx,v 1.6 2018/06/10 17:16:01 smirnovd Exp $
 * Author: M.W. Schulz
 ***************************************************************************
 * Description: calculate and check the CRC
 *      
 *
 *   change log
 *  02-Jul-99 MJL removed default value in args (already done in function
 *                prototypes in CRC.hh
 *
 ***************************************************************************
 * $Log: CRC.cxx,v $
 * Revision 1.6  2018/06/10 17:16:01  smirnovd
 * Add missing namespace OLDEVP
 *
 * Revision 1.5  2007/12/24 06:04:16  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.4  1999/12/07 23:10:30  levine
 * changes to silence the gcc compiler warnings
 *
 * Revision 1.3  1999/07/02 21:07:45  levine
 * removed redundant default value for function parameter, which caused error in
 * Linux gcc compiler. Default value already declared in function prototype
 * in CRC.hh
 *
 * Revision 1.2  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/
#include "CRC.hh" 

using namespace OLDEVP;

#define LSWP0312  ch[0] = cdp[i*4 +3] ; ch[1] = cdp[i*4 + 2] ; ch[2] = cdp[i*4+1] ; ch[3] = cdp[i*4] ;
#define LSWP12    ch[0] = cdp[i*4] ; ch[1] = cdp[i*4 + 2] ; ch[2] = cdp[i*4+1] ; ch[3] = cdp[i*4+3] ;
#define LSWP04    ch[0] = cdp[i*4 +3] ; ch[1] = cdp[i*4 + 1] ; ch[2] = cdp[i*4+2] ; ch[3] = cdp[i*4] ;



/* in memory everything has to be done like this !!!!!*/
/* if you read the data from file, we have to use the variable routine
   for some of the banks */
unsigned int OLDEVP::compute_crc_bank(Bank* d,unsigned int lcrc )
{
  /* first do the header, but not the crc  */
  int i ;
  int* p;
  if(d->header.ByteOrder == 0x04030201)  /* fine no swap */
    {
      p = (int*)&d->header  ;
      for(i = 0 ; i < d->header.BankLength ;i++) CRC32DEF(p[i],lcrc) ;
      return(lcrc);
    }
  else
    {
      int help ;
      int length ;
      char* cdp ;
      char* ch  = (char*)&help ;
      p = (int*)&d->header ;
      cdp = (char*)p ;
      length = (sizeof(Bank_Header)/4) ;
      if(d->header.ByteOrder ==  0x01020304)
	{
	  p = (int*)&d->header ;
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP0312 ; if(i==2)length = help ; CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      if(d->header.ByteOrder ==  0x04030201)
	{
	  p = (int*)&d->header ;
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP12 ;if(i==2)length = help ; CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      if(d->header.ByteOrder ==  0x04030201)
	{
	  p = (int*)&d->header ;
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP04 ;if(i==2)length = help ; CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      return(0xffffff) ;
    }
};
//**************************************************************************
unsigned int OLDEVP::compute_crc_block(int* p,unsigned int ByteOrder,int Length,unsigned int lcrc ) 
{
  int i ;
  if(ByteOrder == 0x04030201)  /* fine no swap */
    {
      for(i = 0 ; i < Length ;i++) CRC32DEF(p[i],lcrc) ;
      return(lcrc);
    }
  else
    {
      int help ;
      int length ;
      char* cdp ;
      char* ch  = (char*)&help ;
      cdp = (char*)p ;
      length = Length ;
      if(ByteOrder ==  0x01020304)
	{
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP0312 ;  CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      if(ByteOrder ==  0x04030201)
	{
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP12 ; CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      if(ByteOrder ==  0x04030201)
	{
	  cdp = (char*)p ;
	  for(i = 0 ; i < length ;i++){ LSWP04 ; CRC32DEF(help,lcrc) ;}
	  return(lcrc) ;
	}
      return(0xffffff) ;
    }
};


