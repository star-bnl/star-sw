/*
 * L3 Reader
 *      
 *
 *   change log
 *
 *************************************************************************** 
*/

//////////////////////////////////////////////  includes  //////////////////////
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
/////////////////////////////////////////////  classes and structures  /////////

class Bank_L3P: public Bank {
public:
  // Put your structure here.

  // You need to override the "Bank" class
  // Swap function if you have any variables
  // that don't swap like integers.  ie. shorts, or char arrays.
};

class L3_Reader {
  friend class EventReader;
public:
  L3_Reader(EventReader *er, Bank_L3P *pL3P);
  ~L3_Reader(){}; 

  // Add whatever you need....
};
