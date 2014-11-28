//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 19, 2006
//

#ifndef JanEventWriter_hh
#define JanEventWriter_hh

// C++ STL
#include <string>
#include <fstream>

// Local
class JanEvent;

class JanEventWriter {
public:
  JanEventWriter(const string& filename = "/dev/stdout");
  ~JanEventWriter();

  ostream& operator()(const JanEvent& event);

private:
  ofstream& out;
};

#endif
