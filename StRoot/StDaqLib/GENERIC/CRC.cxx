/***************************************************************************
 * $Id: CRC.cxx,v 1.2 1999/07/02 04:37:41 levine Exp $
 * Author: M.W. Schulz
 ***************************************************************************
 * Description: calculate and check the CRC
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: CRC.cxx,v $
 * Revision 1.2  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/
#include "CRC.hh" 



#define LSWP0312  ch[0] = cdp[i*4 +3] ; ch[1] = cdp[i*4 + 2] ; ch[2] = cdp[i*4+1] ; ch[3] = cdp[i*4] ;
#define LSWP12    ch[0] = cdp[i*4] ; ch[1] = cdp[i*4 + 2] ; ch[2] = cdp[i*4+1] ; ch[3] = cdp[i*4+3] ;
#define LSWP04    ch[0] = cdp[i*4 +3] ; ch[1] = cdp[i*4 + 1] ; ch[2] = cdp[i*4+2] ; ch[3] = cdp[i*4] ;



/* in memory everything has to be done like this !!!!!*/
/* if you read the data from file, we have to use the variable routine
   for some of the banks */
unsigned int compute_crc_bank(Bank* d,unsigned int lcrc = 0 )
{
  /* first do the header, but not the crc  */
  int i ;
  int* p;
  int iret ;
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
unsigned int compute_crc_block(int* p,unsigned int ByteOrder,int Length,unsigned int lcrc = 0) 
{
  int i ;
  int iret ;
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


