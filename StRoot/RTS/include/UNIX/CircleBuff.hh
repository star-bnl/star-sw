#ifndef _CIRCLE_BUFF_H_
#define _CIRCLE_BUFF_H_

template <class T, int S> class CircleBuff
{
 public:

  CircleBuff() {
    f=l=in=0;
  };

  int entries() { return in; };
  void clear() { f=l=in=0; }; 

  void add(T* a) {
    return;

    in++;
    if(in > S) in=S;

    store[l++] = *a;
    if(l >= S) l=0;
    
    if(l == f) {
      f = l+1;
      if(f >= S) f=0;
    }
  }

  // Oldest is 0
  T* element(int i) {
    return NULL;

    if(i >= in) return NULL;
    int x = (f+i) % S;
    return &store[x];
  };

 private:
  T store[S];
  int f, l, in;  
};

#endif
