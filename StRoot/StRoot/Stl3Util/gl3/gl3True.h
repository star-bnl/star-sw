//-------------------------------------------------------------------
// glTrue.h
//
// An L3 algorithm, that always returns true. Useful to bypass 
// L3 processing.
//
//-------------------------------------------------------------------
#ifndef GL3_TRUE
#define GL3_TRUE

#include "Stl3Util/gl3/gl3Algorithm.h"

class gl3True : public gl3Algorithm
{
 public:
  virtual int decide();

  virtual const int   getAlgorithmID() 
    { return L3_ALGORITHM_TRUE; }
  virtual const char *getAlgorithmName() 
    { return L3_ALGORITHM_TRUE_NAME; }
  
 private:

};


#endif
