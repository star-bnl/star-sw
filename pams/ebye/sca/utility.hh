///////////////////////////////////////////////////////////////////////////////
// utility.hh
///////////////////////////////////////////////////////////////////////////////

#ifndef _utility_included_
#define _utility_included_

#include "Stiostream.h"           // C/C++ header files
#include <assert.h>
#include <math.h>
#include <string.h>


#include "sca_str.hh"

// Type definitions for sca_debug_t and sca_error_t

typedef struct  {
  int print;
  char level[MaxLineLength];
} sca_debug_t;

typedef struct  {
  int print;
  int level;
} sca_error_t;


// function prototypes for the utility functions

sca_debug_t& debug(char *level);
ostream& operator << (ostream& s, sca_debug_t &d);
sca_error_t& error(int level);
ostream& operator << (ostream& s, sca_error_t& e);
int xcomp(const void *a, const void *b);
int ycomp(const void *a, const void *b);

#endif
