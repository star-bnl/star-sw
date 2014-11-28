#ifndef STATICSIZEDDQUEUE_HH
#define STATICSIZEDDQUEUE_HH
// a static allocated dqueue
// values are copied into the list 
// T should have a copy constructor.
// insertion at end of queue is via insert.
// at front of queue via prepend. 
// get copies next element into a and removes the first element
// first returns pointer to first element NO REMOVAL
// clear is clear
// entries returns the number of entries in the list 
// free e returns the number of free elements  

// Modified JML 3/11/99
// Removed size from template, added to constructor  
#include <sys/types.h>
template <class T> class sdqueue
{
public:
  sdqueue(int s);
 ~sdqueue();
  void clear() ;
  int insert(T* a) ;  // returns 0 on success  -1 if list is full (end of list)
  int prepend(T* a) ; // beginning of list 
  int first(T* a) ;       // returns (but doesn't remove) first element 
  int get(T* a) ;    // like first, but removes element 
  u_int entries();   // number of entries in List
  
  u_int free()        // returns number of free entries
  {
    return(max - in) ;
  };  
  
  T* element(int i)
  { 
    if(i < max) return(&store[i]) ;  
    return(NULL) ;
  };

private:
  T *store ;     // array of allocated buffers 
  u_int f ;  // index to first 
  u_int l  ;
  u_int in  ;
  u_int max ; 
  int s;
};

//***********************************************
template <class T> sdqueue<T>::~sdqueue() 
{
  f = l = 0 ;
  in = 0 ; max = 0 ;
  delete [] store;
};

template <class T> sdqueue<T>::sdqueue(int size) 
{
  f = l = 0 ;
  in = 0 ; max = size ;
  s = size;
  store = new T[s];
};

template <class T> void sdqueue<T>::clear()
{
  in = 0 ;
  l = f = 0 ;
};

template <class T> u_int sdqueue<T>::entries()
{
  return(in) ;
};

template <class T> int sdqueue<T>::insert(T* a) // inserts at end 
{
  if(in == max) return(-1) ;
  in++ ;
  store[l++] = *a ;
  if(l == max) l = 0 ;
  return(0) ;
};

template <class T> int sdqueue<T>::get(T* a) // removes first element of list
{
  if(!in)return(-1) ; // empty 
  in-- ;
  *a = store[f++] ; 
  if(f == max) f = 0 ;
  return(0) ;
};

template <class T> int sdqueue<T>::first(T* a) // returns first or null
{
  if(!in) return -1;
  *a = store[f];
  return 0;
};

template <class T> int sdqueue<T>::prepend(T* a) // adds element at the front
{
  if(in == max) return(-1) ;
  in++ ;

  if(in == 1)  
  { // empty list 
    store[f] = *a  ;
    l++ ;
    if(l == max) l = 0 ;
    return(0) ;
  }//end empty list 
  // list is not empty 
  f-- ;
  if(f < 0) f = max - 1 ;
  store[f] = *a ;
  return(0) ;
};

#endif 




