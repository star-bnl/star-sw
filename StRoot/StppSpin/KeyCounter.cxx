//*-- Author : Jan Balewski
//   
// $Id: KeyCounter.cxx,v 1.1.1.2 2001/04/21 00:43:12 fisyak Exp $
// $Log: KeyCounter.cxx,v $
// Revision 1.1.1.2  2001/04/21 00:43:12  fisyak
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:25:38  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// util class, used to cout items with arbitrary ID's                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <stdio.h> 
#include <assert.h> 

#include "KeyCounter.h"


//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void KeyCounter::getBest(int *keym , int *nkeym , int *ntot)
{
  //printf(" KeyC-getBest called \n");
  int  i=0;
  KeyCounter *q;
  *keym=-77777;
  *nkeym=-2;
  *ntot=0;

  for(q=this;; q=q->p)
    {
      if( q==NULL) break;
      i++;
      *ntot+=q->n;
      if(q->n > *nkeym) {*nkeym=q->n; *keym=q->id;}
      //printf("getB deep i=%d, id=%d, ntot=%d, nkeym=%d, keym=%d\n",i, q->id, *ntot, *nkeym, *keym);
    } 

}

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void KeyCounter::add(int key)
{
  //printf(" KeyC-add called, key=%d , id=%d, n=%d \n",key,id,n);
  if(n==0) 
    { id=key; n=1; return;} // init empty list
  if(id==key) 
    {n++;  return;} // increment thie item
  if(p!=NULL){p->add(key);return;} // try add to next item
  p=new KeyCounter(key); // add & init new item
};

//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void KeyCounter::print()
{
  printf(" KeyC-print ID=%d, n=%d add=%d\n",id,n,(int)p);

  if(n==0){ printf(" KeyC -list is EMPTY, add=%d !\n",(int) p); return;};
  if(p!=NULL) p->print();
};


//____________________________________________________________
//____________________________________________________________
//____________________________________________________________
void KeyCounter::clear()
{
  int i=0;
  KeyCounter *pt=p;
  while(pt)
    {
      KeyCounter *tmp=pt;
      pt=pt->p;
      i++;
      //printf("i=%d, %d %d\n",i, (int)pt,(int)pt);
      delete tmp;
    }
  p=NULL;
  id=0;
  n=0;
  //printf(" KeyC-clear  %d nested Itmes, done \n",i);

}












