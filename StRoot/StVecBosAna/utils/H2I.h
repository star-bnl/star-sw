#ifndef H2I_h
#define H2I_h

#include <string>

#include "TH2I.h"

namespace rh {

class H2I : public TH2I
{
public:

   H2I();
   H2I(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, std::string options="");
   H2I(std::string name, std::string title, Int_t nbinsx, const Double_t* xbins, Int_t nbinsy, Double_t ylow, Double_t yup, std::string options="");

   double CalcIntegralAbove(TF1& f1);

   ClassDef(H2I, 1)
};

}

#endif
