#ifndef H1I_h
#define H1I_h

#include <string>

#include "TH1I.h"

namespace rh {

class H1I : public TH1I
{
public:

   H1I();
   H1I(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, std::string options="");
   H1I(std::string name, std::string title, Int_t nbinsx, const Double_t* xbins, std::string options="");

   ClassDef(H1I, 1)
};

}

#endif
