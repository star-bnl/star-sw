// implementation of singly linked lists
// especially THitList and TTrackList
// methods:
//	first()		- return slink of first item
//	inf(slink)	- retrun Contents of slink
//	succ(slink)	- return next slink or NULL at end of list
//	append(item)- append item to list
//	tail()		- return contents of last item
//	head()		- return contents of first item
//	size()		- return size of list
//	clear()		- remove all entries (doesnt't delete the entries!)
//	conc(slist)	- append another list

#ifndef __lists_inc
#define __lists_inc


#include <stdlib.h>

#ifdef LEDA
#include "_memory.hpp"
#endif

class slink;

typedef slink* slist_item;

class slink 
{
public:
  slink* succ;
  void* e;

  slink(void* a, slink* suc) { e = a; succ = suc; };
  slink* operator++(){ return succ ; } ; 
#ifdef LEDA
    LEDA_MEMORY(slink)
#endif
};

template <class T> class slist
{
private:
   slink* h;                     //head
   slink* t;                     //tail
   int    count;                 //length of List

public:
   slist() {h = 0; t = 0; count = 0;};
   ~slist() 
   {
     if (h!=0) {
        register slink* p, *old;
        for(p = h; p; ) {
// get next element before deleting this one
           old = p;
           p = p->succ;
           delete old;
         }
         h=t=0;
         count=0;
      }
   };
   slink* first() {return h;};
   const T& inf(slink* item) const { return ((T&) (item->e));};
   T inf(slink* item)  {return (T) (item->e);};
   slink* succ(slink* item) const {return item->succ;};
   slink* append(T a)
   { count++;
      if (t) t = t->succ = new slink((void*)a,0); 
      else   t = h = new slink((void*)a,0); 
      return t;
   };
   void remove(slink* item)
   {
      slink* temp;
      count--;
// search succ(el) == item
      if (item == h) {
         h = item->succ;
         if (h == 0) {
            t = 0;
            count = 0;
         }
         delete item;
      }
      else
         for(temp = h; temp != 0; temp = temp->succ)
         if (temp->succ == item) {
// found, relink and remove item
            temp->succ = item->succ;
            if (temp->succ == 0) t = temp;
            delete item;
            break;
       }
   };

   const T head() const {return (T)h->e;};
   const T tail() const {return (T)t->e;};
   const T itsObject  ( slink* &s ) { if ( s == 0 ) return 0 ;
                                     else return (T)s->e;} ;
   const T nextObject ( slink* &s ) { if ( s == 0 ) return 0 ; 
                                     else {
                                     s = s->succ ;
                                     if ( s != 0 )  return (T)s->e;
                                     else     return 0 ; } 
                                     } ;
   int size() {return count;};

   void clear() {
      if (h!=0) {
         register slink* p, *old;
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

   void conc(slist<T>& l)
   { 
      if (count > 0) 
      { t->succ = l.h;
         if (l.count > 0) t = l.t; 
      }
      else 
	{ h = l.h; 
	t = l.t; 
      }
      count = count+l.count;
      l.h = l.t = 0;
      l.count = 0;
   };
#ifdef LEDA
     LEDA_MEMORY(slist)
#endif
};

#ifdef LEDA
#if defined(__GNUC__) || defined(__ELSE_SCOPE_BUG__)
#define LEDA_FORALL_PREAMBLE
#define LOOP_CAT(x,y) x ## y 
#define LOOP_VAR(y)  LOOP_CAT(LOOP_VAR,y)
#define LOOP_VAR1(y) LOOP_CAT(LOOP_VAR1,y)
#else
#define LEDA_FORALL_PREAMBLE  if (0); else
#define LOOP_VAR(y)  LOOP_VAR
#define LOOP_VAR1(y) LOOP_VAR1
#endif

#define LOOP_ITEM(p) (slink*)p

#if defined(__NO_LOCAL_TYPEDEF__)
template <class T>
inline T cast_to_item(T,void* p) { return (T)p; }
#undef  LOOP_ITEM
#define LOOP_ITEM(p) cast_to_item(S.first(),p)
#endif

template<class T, class var_type>
inline void LedaLoopInf(const T& S, var_type& x, void* p) 
{ if (p) x = S.inf(LOOP_ITEM(p)); }

template<class T>
inline int LedaLoopSucc(const T& S, void*& p) 
{ if (p) { p = S.succ(LOOP_ITEM(p)); return 1; }
  else return 0;
}

#define forall(x,S)\
LEDA_FORALL_PREAMBLE \
for(void* LOOP_VAR(__LINE__) = (S).first();\
LedaLoopInf(S,x,LOOP_VAR(__LINE__)), LedaLoopSucc(S,(void*)LOOP_VAR(__LINE__)); )
#else
#define forall(x,S)\
slink* link = (S).first();\
for( x = (S).itsObject(link); link != 0; x = (S).nextObject(link) )
  
#endif

#endif
