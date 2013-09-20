#ifndef SIMPLEQ2_HH
#define SIMPLEQ2_HH
// Same as simpleQ, except the size of the queue is not part of the template
// but rather set in the constructor!
#include <stdlib.h>
#include <sys/types.h>

template <class T> class simpleQ2
{
public:
    simpleQ2(int sz);
    ~simpleQ2();

    void clear();
    int put(T* a) ;     // returns 0 on success  -1 if list is full (end of list)
    int prepend(T* a) ; // beginning of list
    int get(T* a) ;     // like first, but removes element 
    int query(T* a) ;

    int entries()      // number of entries in List
    {
	return in;
    };

    int free()        // returns number of free entries
    {
	return(S - in) ;
    };  
  
    T* element(int i)
    { 
	if(i < S) return(&store[i]) ;  
	return(NULL) ;
    };

    //private:
    int S;
    T *store ;     // array of allocated buffers 
    int f ;  // index to first 
    int l;
    int in;
};

//***********************************************
template <class T> simpleQ2<T>::simpleQ2(int sz) 
{
    f = l = 0 ;
    in = 0 ;
    S = sz;
    store = NULL;
    store = (T *)malloc(sizeof(T) * S);
};

template <class T> simpleQ2<T>::~simpleQ2()
{
    if(store) ::free(store);
}

template <class T> void simpleQ2<T>::clear()
{
    in = 0 ;
    l = f = 0 ;
};

template <class T> int simpleQ2<T>::put(T* a) // inserts at end 
{
    if(in == S) return(-1) ;
    in++ ;
    store[l++] = *a ;
    if(l == S) l = 0 ;
    return(0) ;
};

template <class T> int simpleQ2<T>::get(T* a) // removes first element of list
{
    if(!in)return(-1) ; // empty 
    in-- ;
    *a = store[f++] ; 
    if(f == S) f = 0 ;
    return(0) ;
};

template <class T> int simpleQ2<T>::prepend(T* a) // adds element at the front
{
    if(in == S) return(-1) ;
    in++ ;

    f--;
    if(f<0) f = S - 1;
    store[f] = *a;

    return 0;
};

template <class T> int simpleQ2<T>::query(T* a)
{
    int i=f;
  
    while(i != l) 
	{
	    if(memcmp(a,&store[i],sizeof(T)) == 0) return 1;
	    i++;
	    if(i==S) i=0;
	}

    return 0;
};

#endif 







