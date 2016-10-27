#ifndef H1D_h
#define H1D_h

#include <string>

#include "TH1D.h"

namespace rh {

class H1D : public TH1D
{
public:

   H1D();
   H1D(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, std::string options="");
   H1D(std::string name, std::string title, Int_t nbinsx, const Double_t* xbins, std::string options="");

   ClassDef(H1D, 1)
};

}

#endif
