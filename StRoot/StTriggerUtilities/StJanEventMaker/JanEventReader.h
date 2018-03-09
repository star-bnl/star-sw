//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 18, 2006
//

#ifndef JanEventReader_hh
#define JanEventReader_hh

// C++
#include <string>
#include <fstream>

// Local
class JanEvent;

class JanEventReader {
public:
  JanEventReader(const string& filename = "/dev/stdin");
  ~JanEventReader();

  istream& operator()(JanEvent& event);
  bool isOpen(){ return in.is_open();}

private:
  ifstream& in;
};

#endif
