#include "H1D.h"

ClassImp(rh::H1D)

namespace rh {

using namespace std;


H1D::H1D() : TH1D()
{
}


H1D::H1D(string name, string title, Int_t nbinsx, Double_t xlow, Double_t xup, string options) :
   TH1D(name.c_str(), title.c_str(), nbinsx, xlow, xup)
{
   SetOption(options.c_str());
}


H1D::H1D(string name, string title, Int_t nbinsx, const Double_t* xbins, string options) :
   TH1D(name.c_str(), title.c_str(), nbinsx, xbins)
{
   SetOption(options.c_str());
}

}
