#ifndef G_INPUT_H
#define G_INPUT_H

#include <RQ_OBJECT.h>

namespace Garfield {

  int ReadInteger(char* token, int def, bool& error);
  double ReadDouble(char* token, double def, bool& error);
  
}

#endif
