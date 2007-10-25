//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 18, 2006
//

// Local
#include "JanEvent.h"
#include "JanEventReader.h"

JanEventReader::JanEventReader(const string& filename)
  : in(*new ifstream(filename.c_str()))
{
  if (!in) cerr << "Can't open " << filename << endl;
}

JanEventReader::~JanEventReader()
{
  delete& in;
}

istream& JanEventReader::operator()(JanEvent& event)
{
  return in >> event;
}
