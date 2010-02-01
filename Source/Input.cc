#include <stdio.h>
#include "Input.hh"

namespace Garfield {

  int ReadInteger(char* token, int def, bool& error) {
  
    if (!token) {
      error = true;
      return def;
    } else {
      return atoi(token);
    }
  }
  
  double ReadDouble(char* token, double def, bool& error) {
  
    if (!token) {
      error = true;
      return def;
    } else {
      return atof(token);
    }
    
  }
  
}
