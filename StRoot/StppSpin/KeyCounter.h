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
