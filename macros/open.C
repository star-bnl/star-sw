#include <fstream>
#include "Riostream.h"
Bool_t open(const Char_t *filename) {
  std::ifstream infile;
  infile.open(filename, std::ios::in);
  // Make sure the file could actually be opened.
  if (!infile) {
    std::cout << "    Error opening file\n";
    std::cout << "    " << filename << ".\n";
    return kFALSE;
  }
  return kTRUE;
}
