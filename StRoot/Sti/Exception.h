#ifndef Exception_HH
#define Exception_HH 1

#include <iostream>
using std::cout;
using std::endl;
using std::ostream;

#include <string>
using std::string;

class Exception
{
 public:

  Exception(const string& s);
  virtual ~Exception();
  friend ostream & operator << (ostream &ostr, Exception &e);

 protected: 

  string  text;
};

#endif
