// implementation of singly linked lists
// especially THitList and TTrackList
// methods:
//	first()		- return thmLink of first item
//	inf(thmLink)	- retrun Contents of thmLink
//	succ(thmLink)	- return next thmLink or NULL at end of list
//	append(item)- append item to list
//	tail()		- return contents of last item
//	head()		- return contents of first item
//	size()		- return size of list
//	clear()		- remove all entries (doesnt't delete the entries!)
//	conc(thmList)	- append another list

#ifndef __thmLists_inc
#define __thmLists_inc


#include <stdlib.h>

class thmLink;

typedef thmLink* thmList_item;

class thmLink 
{
public:
  thmLink* succ; 
  thmLink* pred;
  void* e;


  thmLink(void* a, thmLink* pre, thmLink* suc) { e = a; succ = suc; pred = pre; };
  thmLink* operator++(){ return succ ; } ; 
  thmLink* operator--(){ return pred;};
#ifdef LEDA
    LEDA_MEMORY(thmLink)
#endif
};

template <class T> class thmList
{
private:
   thmLink* h;                     //head
   thmLink* t;                     //tail
   int    count;                 //length of List

public:
   thmList() {h = 0; t = 0; count = 0;};
   ~thmList() 
   {
	clear();
   };					// !!!!!!!!!!!!!!!!!! definieren!!!!!!!!!
  thmLink* first() {return h;};
  thmLink* last() {return t;};
  const T& inf(thmLink* item) const { return ((T&) (item->e));};
  T inf(thmLink* item)  {return (T) (item->e);};
  thmLink* succ(thmLink* item) const {return item->succ;};
  thmLink* append(T a)
  { count++;
    if (t) {
      t->succ = new thmLink((void*)a,t,0) ;
      t = t->succ ;
    }  
    else   t = h = new thmLink((void*)a,0,0); 
    return t;
  };

  void remove(thmLink* item)
    {
      if (item == h) {
	h = item->succ;
	h->succ->pred=h;
	h->pred=0;
      } else if (item == t) {
	t = item->pred;
	t->pred->succ=t;
	t->succ=0;
      } else {
	item->pred->succ = item->succ ;
	item->succ->pred = item->pred ;
      }

      delete item;
      
      count--;
    }
  

   const T head() const {return (T)h->e;};
   const T tail() const {return (T)t->e;};
//   const T itsObject  ( thmLink* &s ) { if ( s == 0 ) return 0 ;
//                                     else return (T)s->e;} ;
//
//   const T nextObject ( thmLink* &s ) { if ( s == 0 ) return 0 ; 
//                                     s = s->succ ;
//                                     if ( s != 0 )  return (T)s->e;
//                                     else     return 0 ; } ;
      
   int size() {return count;};
   void clear() {
      if (h!=0) {
         register thmLink* p, *old;
         for(p = h; p; ) {
// get next element before deleting this one
            old = p;
            p = p->succ;
            delete old;
          }
          h=t=0;
          count=0;
      }
   }

};

  
#define forall(x,S)\
thmLink* link = (S).first();\
for( x = (S).itsObject(link); link != 0; x = (S).nextObject(link) )


#endif





