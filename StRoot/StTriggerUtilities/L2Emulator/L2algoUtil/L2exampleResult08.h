//
// Jan Balewski ,MIT
// Example container for results from L2-algo
//
// L2Result must have a size N*4 char, 
//

#ifndef L2_EXAMPLE_RESULT_08_H
#define L2_EXAMPLE_RESULT_08_H

struct L2exampleResult08 {
  enum {mySizeChar=16};// negotiate the  size before extending it
  unsigned short val1, val2;
  float val3;
};

#endif
