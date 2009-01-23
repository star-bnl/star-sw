#ifndef STAR_evpMainPresenter
#define STAR_evpMainPresenter

#include "TObject.h"

class evpMainPresenter : public TObject 
{
   private:
       evpMainPresenter(){}  
   public:
      virtual ~evpMainPresenter(){}
      static int main(int argc, char **argv );
      ClassDef(evpMainPresenter,0);
};
#endif
