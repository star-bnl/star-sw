#ifndef _QTTEST_H_
#define _QTTEST_H_

#include "TObject.h"

extern const char *testCurrFile;
extern int testCurrLine;

#define CP testCurrFile=__FILE__;testCurrLine=__LINE__

class QtTest : public TObject 
{
 private:
  QtTest() {
  }  

 public:
  virtual ~QtTest(){}
  static int _main(int argc, char **argv );
  static int main(char *args);
  ClassDef(QtTest,0);
};

extern QtTest *qttest;

#endif
