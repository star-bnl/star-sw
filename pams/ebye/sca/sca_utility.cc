////////////////////////////////////////////////////////////
//
// utility.cc
//
// These are general utility functions used by a variety
// of classes.
//

#include <fstream.h>
#include <string.h>
#include <stdlib.h>
#include "utility.hh"


////////////////////////////////////////////////////////////
// debug()
// This function returns a debug_t structure which is used
// by the overloaded << operator to decide whether to print
// something or not.
// Usage example:
//   cout << debug("Calc_Entropy") << "calc_entropy() called.\n";
// will output the message if Calc_Entropy or SCALE_DEBUG
// is defined.

debug_t& debug(char *level)
{
  static debug_t d;
  d.print = FALSE;
  return d;
}  // end debug()

ostream& operator << (ostream& s, debug_t& d)
{
  static int init=0;
  static ostream *blank;
  if(!init) {
    blank = new ofstream("/dev/null");
    init=7;
  }
  
  if( d.print )
    {
      s << d.level << " : ";
      return s;
    }
  else
    return *blank;
}  // end << overloading for debug_t


////////////////////////////////////////////////////////////
// error()
// This prints out an error message based upon the value
// of SCALE_ERRORS.  Default is LOW.  Which ever level
// is picked, errors of that level and above are printed.
// SILENT : Print no error messages.
// HIGH   : Errors for which SCALE does not know how to
//          recover, but not necessarily fatal.
// MEDIUM : Errors for which SCALE makes an intelligent
//          guess about what to do.
// LOW    : General warnings but by no means critical.
// Usage example:
//   cerr << error(MEDIUM) << "Using default array size.\n";
// will print the message as an error if SCALE_ERRORS is
// defined to LOW, MEDIUM, or not defined, but will not
// print anything if SCALE_ERRORS is HIGH or SILENT.

error_t& error(int level)
{
  static int first_time = TRUE;
  static int print_level;
  static error_t e;

  e.level = level;		// save level every time

  if(first_time)
    {
      first_time = FALSE;
      char *buf = getenv("SCALE_ERRORS");
      if(buf)
	{
	  if( !strcmp(buf, "HIGH") )
	    print_level = HIGH;
	  else if( !strcmp(buf, "MEDIUM") )
	    print_level = MEDIUM;
	  else if( !strcmp(buf, "LOW") )
	    print_level = LOW;
	  else if( !strcmp(buf, "SILENT") )
	    print_level = SILENT;
	  else
	    print_level = LOW;
	}
      else
	{
	  print_level = LOW;
	  cerr << "SCALE_ERRORS not defined.  Using default of LOW.\n";
	}
    }  // end first time;
  
  e.print = FALSE;
  if(level >= print_level && print_level != SILENT)
    e.print = TRUE;
  
  return e;
}  // end error()

ostream& operator << (ostream& s, error_t& e)
{
  static int init=0;
  static ostream *blank;
  if(!init) {
    blank = new ofstream("/dev/null");
    init=7;
  }

  if( e.print )
    {
      if(e.level == LOW)
	s << "Warning : ";
      else
	s << "Error : ";
      return s;
    }
  else
    return *blank;
}  // end << overloading for error_t


////////////////////////////////////////////////////////////
// xcomp(), ycomp()
// Comparisons of sca_data_t->x and sca_data_t->y for sorts,
// searches, etc.

int xcomp(const void *a, const void *b)
{
  if( (*(sca_data_t **)a)->x < (*(sca_data_t **)b)->x )
    return -1;
  else
    return 1;
}

int ycomp(const void *a, const void *b)
{
  if( (*(sca_data_t **)a)->y < (*(sca_data_t **)b)->y )
    return -1;
  else
    return 1;
}
 

