/***************************************************************************
 * $Id: RecHeaderFormats.cxx,v 1.8 2018/06/10 17:16:01 smirnovd Exp $
 * Author: M.W. Schulz, Jeff Landgraf, M.J. LeVine
 ***************************************************************************
 * Description: Bank header formats common to all detectors in STAR:
 *              methods
 *
 * History of modifications
 * 06/11/99
 *  Loop index changed from l to ll to compensate for the problems this
 *  compiler has with the scope ov variables that are in parentheses
 *  Place in the code marked with the above comment.
 *
 ***************************************************************************
 * $Log: RecHeaderFormats.cxx,v $
 * Revision 1.8  2018/06/10 17:16:01  smirnovd
 * Add missing namespace OLDEVP
 *
 * Revision 1.7  2014/06/25 15:33:16  jeromel
 * Code not used but erradicated use of flush
 *
 * Revision 1.6  2007/12/24 06:04:17  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.5  1999/12/07 23:10:31  levine
 * changes to silence the gcc compiler warnings
 *
 * Revision 1.4  1999/07/04 01:47:59  levine
 * minor changes to make solaris CC compiler happy
 *
 * Revision 1.3  1999/07/02 04:37:41  levine
 * Many changes - see change logs in individual programs
 *
 *
 **************************************************************************/



#include <stdio.h>
#include <assert.h>
#include <sys/types.h>

#include "RecHeaderFormats.hh"
#include "CRC.hh"
#include "swaps.hh"
#include <string.h>

#define NOCRCCHECKING

// Functions valid on all BANKS
using namespace OLDEVP;

char* OLDEVP::name2str(char* type) /* for everyone !!!! */
{
  static char name[9] = {0}  ;
  memcpy(name,type,8) ;
  name[8] = 0 ;
  return(name) ;
};

// first some trash can functions 
void OLDEVP::dump_data(char* buffer,int size,int width )
{
  uint* data_base = (uint*)buffer ;
  uint* data ;
  char* data_char ;
  
  if(!size) return ; 
  data = data_base;
  data_char = (char*) data ;
  for(int w = 0 ; w < size ; w++)
    {
      if((w > 0) && (!(w%width)))
	{
	  printf(" || ");
	  data_char = (char*)(&data[w - width]);
	  for(int l = 0 ; l  < width*4 ; l++)
	    { 
	      if(((uint)data_char[l]) < 0x21  || ((uint)data_char[l]) > 0x176)
		printf(" ") ;
	      else
		printf("%c",data_char[l]) ;
	    }
	  printf("\n") ;
	}
      printf("0X%08X ",data[w]) ;
    }
  int left_over =size%width ;
  if(!left_over)
    {
      printf(" || ") ;
      data_char = (char*)(&data[size - width ] ) ;
      for(int l = 0 ; l < width*4 ; l++)
	{
	  if(((uint)data_char[l]) < 0x21  || ((uint)data_char[l]) > 0x176)
	    printf(" ") ;
	  else
	    printf("%c",data_char[l]) ;
	}
      printf("\n") ;
    }
  if(left_over)
    {
      //Loop index changed from l to ll to compensate for the problems this
      //compiler has with the scope ov variables that are in parentheses
      for(int ll = 0 ; ll < (width - left_over) ; ll++) printf("           ") ;
      printf(" || ") ;
      data_char = (char*)(&data[size - left_over ] ) ;
      for(int l = 0 ; l < left_over*4 ; l++)
	{
	  if(((uint)data_char[l]) < 0x21  || ((uint)data_char[l]) > 0x176)
	    printf(" ") ;
	  else
	    printf("%c",data_char[l]) ;
	}
      printf("\n") ;
    }
} 
/*---------------------------------------------------------------------------*/

//***********************************************
int Logical_Record_Header::swap()
{
  // start only at the bank length the BankType character array not swapped
  int iret =  swap_raw(ByteOrder,&BankLength,
		       ((sizeof(Logical_Record_Header))/4  - 2));
  return(iret) ;
};

void Logical_Record_Header::print()
{
  if(ByteOrder !=  0x04030201)
    {
      Logical_Record_Header lbh ;
      memcpy(&lbh , this ,sizeof(lbh)) ;
      int iret = lbh.swap() ;
      if(iret < 0 ) printf("swapping failed %s %d\n",__FILE__,__LINE__) ;
      lbh.print() ;
      return ;
    }
  printf("Logical Record Header:\n");
  printf("BankType   Length   RunNum  FormatVersion   ByteOrder           RES1   RES2    RES3          CRC\n") ;
  printf("%-8s   %06d    %03d     %04d.%04d      0X%08X      %04d      %05d  0X%08X   0X%08X\n",
	 name2str(BankType),BankLength,RunNumber,FormatVersion >> 16 ,
	 FormatVersion & 0x0000FFFF, ByteOrder, reserved1, 
	 reserved2, reserved3 , CRC) ;

};

// Does not check or reset the CRC
int Logical_Record::swap()
{
  // get the length and swap it
  int words = header.BankLength ;
  int iret = swap_raw(header.ByteOrder,&words,1) ;

  // swap returns zero means no swapping needed
  if(!iret) return(0);

  // swap returns less than zero means error
  if(iret < 0 ) return(iret) ;

  // swap the body first... don't want to overwrite the byte order to early
  // Don't swap record type    
  iret = swap_raw(header.ByteOrder,&RecordLength,2);
  assert(iret > 0);                         // checked the byte order above
  iret = swap_raw(header.ByteOrder,&CRC,1);  
  assert(iret > 0);

  // swap the header
  header.swap();

  return(iret) ;
};

// For the purposes of the header CRC LRH // LR is just another bank
void Logical_Record::set_CRC()
{
  header.CRC = 0 ; 
  header.CRC = compute_crc_bank((Bank *)this) ;
};

int Logical_Record::test_CRC() 
{
#ifdef NOCRCCHECKING
  return -1;
#endif

  if(header.CRC == 0) return(-1) ;
  unsigned int old_crc = header.CRC ;
  unsigned int test = compute_crc_bank((Bank *)this) ;
  header.CRC = old_crc ;  // reconstruct the old CRC 
  swap_raw(header.ByteOrder ,(int*)&old_crc, 1);
  if(old_crc != test) return(0) ;
  return(-1) ;
}; 

void Logical_Record::print(int level)
{
  header.print();
  
  int words = header.BankLength ;
  swap_raw(header.ByteOrder,&words,1);

  dump_data(((char *)this) + sizeof(header),words - sizeof(header)/4);
}
  
//***********************************************
int Bank_Header::swap()
{
  // start only at the bank length the BankType character array not swapped
  int iret =  swap_raw(ByteOrder,&BankLength,((sizeof(Bank_Header))/4  - 2));
  return(iret) ;
};

void Bank_Header::print()
{
  if(ByteOrder !=  0x04030201)
    {
      Bank_Header lbh ;
      memcpy(&lbh , this ,sizeof(lbh)) ;
      int iret = lbh.swap() ;
      if(iret < 0 )printf("swapping failed %s %d\n",__FILE__,__LINE__) ;
      lbh.print() ;
      return ;
    }
  printf("Bank Header:\n");
  printf("BankType   Length   BankId  FormatVersion   ByteOrder   FormatNumber  Token     RES          CRC\n") ;
  printf("%-8s   %06d    %03d     %04d.%04d      0X%08X      %04d      %05d  0X%08X   0X%08X\n",
	 name2str(BankType),BankLength,BankId,FormatVersion >> 16 ,
	 FormatVersion & 0x0000FFFF, ByteOrder, FormatNumber, 
	 Token, reserved1 , CRC) ;

};

//*****************************************************
// Override for those banks which require byte or 
// integer swapping
//
// Does not check or reset the CRC
int Bank::swap()
{
  // get the length and swap it
  int words = header.BankLength ;
  int iret = swap_raw(header.ByteOrder,&words,1) ;

  // swap returns zero means no swapping needed
  if(!iret) return(0);

  // swap returns less than zero means error
  if(iret < 0 ) return(iret) ;

  // swap the body first... don't want to overwrite the byte order to early
  words -= (sizeof(header)/4) ;
  int* start = (int*)this + (sizeof(header)/4) ;
  iret = swap_raw(header.ByteOrder,start,words) ;
  assert(iret > 0);  // checked the byte order above

  // swap the header
  header.swap();

  return(iret) ;
};

void Bank::set_CRC()
{
  header.CRC = 0 ; 
  header.CRC = compute_crc_bank(this) ;
};

int Bank::test_CRC() 
{
#ifdef NOCRCCHECKING
  return -1;
#endif

  if(!header.CRC)return(-1) ;
  unsigned int old_crc = header.CRC ;
  unsigned int test = compute_crc_bank(this) ;
  header.CRC = old_crc ;  // reconstruct the old CRC 

  swap_raw(header.ByteOrder ,(int*)&old_crc, 1);
  if(old_crc != test) return(0) ;
  return(-1) ;
}; 

void Bank::print(int level)
{
  header.print();
  
  int words = header.BankLength ;
  swap_raw(header.ByteOrder,&words,1);

  printf( "XXXXXX:  length= %d, %d\n",  words, words*4);
  dump_data(((char *)this) + sizeof(header),words - sizeof(header)/4);
}





