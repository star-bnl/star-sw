///////////////////////////////////////////////////////////////////////////////
// utility.hh
///////////////////////////////////////////////////////////////////////////////

#ifndef _utility_included_
#define _utility_included_

#include <iostream.h>           // C/C++ header files
#include <assert.h>
#include <math.h>
#include <string.h>


#include "sca_str.hh"

// Type definitions for debug_t and error_t

typedef struct debug_t {
  int print;
  char level[MaxLineLength];
} debug_t;

typedef struct error_t {
  int print;
  int level;
} error_t;


// function prototypes for the utility functions

debug_t& debug(char *level);
ostream& operator << (ostream& s, debug_t &d);
error_t& error(int level);
ostream& operator << (ostream& s, error_t& e);
int xcomp(const void *a, const void *b);
int ycomp(const void *a, const void *b);

#endif
