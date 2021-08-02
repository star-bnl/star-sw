#ifndef STAR_endOfRunActionMain
#define STAR_endOfRunActionMain

#include "TObject.h"

class endOfRunActionMain : public TObject
{
   private:
      endOfRunActionMain() {}
   public:
      virtual ~endOfRunActionMain() {}
      static int main(int argc, const char* argv[]);
      ClassDef(endOfRunActionMain,0);
};

#endif
