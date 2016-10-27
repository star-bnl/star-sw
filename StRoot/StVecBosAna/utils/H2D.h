#ifndef H2D_h
#define H2D_h

#include <string>

#include "TH2D.h"

namespace rh {

class H2D : public TH2D
{
public:

   H2D();
   H2D(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, std::string options="");
   H2D(std::string name, std::string title, Int_t nbinsx, const Double_t* xbins, Int_t nbinsy, Double_t ylow, Double_t yup, std::string options="");

   ClassDef(H2D, 1)
};

}

#endif
