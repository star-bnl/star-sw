#include "OTH.h"


// static members*
OTH* OTH::_instance = 0;

// static methods
OTH* OTH::instance() {
  if ( !_instance ) {
    _instance = new OTH();
  }
  return _instance;
}




