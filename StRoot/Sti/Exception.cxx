
#include "Exception.h"

Exception::Exception(const string& t)
{
  text = t;
}

Exception::~Exception()
{
  //delete [] text;
}

ostream & operator << (ostream &ostr, Exception &e)
{
  ostr << e.text << endl;
  return ostr;
}
