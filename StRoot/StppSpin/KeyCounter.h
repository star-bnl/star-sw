//*-- Author : Jan Balewski
//  
// $Id: KeyCounter.h,v 1.1.1.2 2001/04/21 00:43:13 fisyak Exp $
// $Log: KeyCounter.h,v $
// Revision 1.1.1.2  2001/04/21 00:43:13  fisyak
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

#ifndef JBAL_KEYCOUNTER_H
#define JBAL_KEYCOUNTER_H
/*
 this class is designed to count how many entries with different keys 
 were accumulated
*/
#include <stdlib.h>

class KeyCounter
{
 private: //.........................................
  int id;
  int n;
  //int aa[1000000];
  KeyCounter *p;

 public: //..........................................
  KeyCounter(){id=0;n=0;p=NULL;};
  KeyCounter(int key){id=key;n=1;p=NULL;};
  ~KeyCounter(){ /* printf("dest, id=%d\n",id); */};
  void print();
  void clear();
  void add(int ); 
  void getBest(int * , int * , int *);

};

#endif
