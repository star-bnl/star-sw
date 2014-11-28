
#include "l3BankUtils.h"
#include "l3Swap.h"

#include <stdlib.h>
#include <string.h>

void *offlen2ptr(void *base, struct offlen ol)
{
    int off, len;
    
    if( ((bankHeader*)base)->byte_order == DAQ_RAW_FORMAT_ORDER) {
	off=ol.off;
	len=ol.len;
    } else {
	off=swap32(ol.off);
	len=swap32(ol.len);
    }
  
    void *ptr;
    if (len > 0)
	ptr = (void *)((char *)base + off*4);
    else
	ptr = (void *)0;
    
    return ptr;
}



void *offlen2ptr(void *base, struct offlen ol, char *type)
{
    
    void *ptr = offlen2ptr(base, ol);
    
    return validate(ptr, type);
}



void *validate(void * ptr, char *type)
{
  if(validateBank(ptr, type))
    return ptr;
  else
    return NULL;
};


bool validateBank(void * ptr, char *type)
{
  struct bankHeader *bh =(struct bankHeader *) ptr;
  int typelen;
  for(typelen = 1; typelen<8 && type[typelen-1]!=' ';typelen++);
  //  cout << type << "." << typelen<< endl;

  if(ptr == NULL)
      return false;

  else if (strncmp(type, bh->bank_type, typelen)) // types don't match
    {
// 	if (logLevel >= error)
// 	    cerr << "L3Event: Invalid BankHeader (" << bh->bank_type
// 		 << " instead of " << type << ")" << endl;
	return false;
    }
  else
    return true;
}

int bankSize(bankHeader bh)
{
    if (bh.byte_order == DAQ_RAW_FORMAT_ORDER) {
	return (bh.length * 4);
    } else {
	return (swap32(bh.length) * 4);
    }

}
