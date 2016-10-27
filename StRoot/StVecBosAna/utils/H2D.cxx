#include "H2D.h"

ClassImp(rh::H2D)

namespace rh {

using namespace std;


H2D::H2D() : TH2D()
{
}


H2D::H2D(string name, string title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, string options) :
   TH2D(name.c_str(), title.c_str(), nbinsx, xlow, xup, nbinsy, ylow, yup)
{
   SetOption(options.c_str());
}


H2D::H2D(string name, string title, Int_t nbinsx, const Double_t* xbins, Int_t nbinsy, Double_t ylow, Double_t yup, string options) :
   TH2D(name.c_str(), title.c_str(), nbinsx, xbins, nbinsy, ylow, yup)
{
   SetOption(options.c_str());
}

}
