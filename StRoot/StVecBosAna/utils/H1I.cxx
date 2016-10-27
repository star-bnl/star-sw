#include "H1I.h"

ClassImp(rh::H1I)

namespace rh {

using namespace std;


H1I::H1I() : TH1I()
{
}


H1I::H1I(string name, string title, Int_t nbinsx, Double_t xlow, Double_t xup, string options) :
   TH1I(name.c_str(), title.c_str(), nbinsx, xlow, xup)
{
   SetOption(options.c_str());
}



H1I::H1I(string name, string title, Int_t nbinsx, const Double_t* xbins, string options) :
   TH1I(name.c_str(), title.c_str(), nbinsx, xbins)
{
   SetOption(options.c_str());
}

}
