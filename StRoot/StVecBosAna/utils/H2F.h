#ifndef H2F_h
#define H2F_h

#include <string>

#include "TH2F.h"

namespace rh {

class H2F : public TH2F
{
public:

   H2F();
   H2F(std::string name, std::string title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, std::string options="");

   double CalcIntegralAbove(TF1& f1);

   ClassDef(H2F, 1)
};

}

#endif
