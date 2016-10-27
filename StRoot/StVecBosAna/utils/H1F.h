#ifndef H1F_h
#define H1F_h

#include <string>

#include "TH1F.h"

namespace rh {

class H1F : public TH1F
{
public:

   H1F();
   H1F(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, std::string options="");

   ClassDef(H1F, 1)
};

}

#endif
