#include "H1F.h"

ClassImp(rh::H1F)

namespace rh {

using namespace std;


H1F::H1F() : TH1F()
{
}


H1F::H1F(string name, string title, Int_t nbinsx, Double_t xlow, Double_t xup, string options) :
   TH1F(name.c_str(), title.c_str(), nbinsx, xlow, xup)
{
   SetOption(options.c_str());
}

}
