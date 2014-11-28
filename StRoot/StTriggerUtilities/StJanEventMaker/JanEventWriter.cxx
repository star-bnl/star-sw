//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 19, 2006
//

// Local
#include "JanEvent.h"
#include "JanEventWriter.h"

JanEventWriter::JanEventWriter(const string& filename)
  : out(*new ofstream(filename.c_str()))
{
  if (!out) cerr << "Can't open " << filename << endl;
}

JanEventWriter::~JanEventWriter()
{
  delete& out;
}

ostream& JanEventWriter::operator()(const JanEvent& event)
{
  return out << event;
}
