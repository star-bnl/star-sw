#ifndef Exception_HH
#define Exception_HH 1

#include <iostream.h>
#include <string>

class Exception
{
 public:

  Exception(const char *s);
  virtual ~Exception();
  friend ostream & operator << (ostream &ostr, Exception &e);

 protected: 

  string  text;
};

#endif
