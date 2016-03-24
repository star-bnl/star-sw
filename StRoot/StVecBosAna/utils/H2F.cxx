#include "H2F.h"

#include "TF1.h"
#include "TList.h"
#include "TText.h"

ClassImp(rh::H2F)

namespace rh {

using namespace std;


H2F::H2F() : TH2F()
{
}


H2F::H2F(string name, string title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, string options) :
   TH2F(name.c_str(), title.c_str(), nbinsx, xlow, xup, nbinsy, ylow, yup)
{
   SetOption(options.c_str());
}


/**
 * Computes integral value of all bin above the curve given by the function f1.
 */
double H2F::CalcIntegralAbove(TF1& f1)
{
   int xfirst = fXaxis.GetFirst();
   int xlast  = fXaxis.GetLast();
   int yfirst = fYaxis.GetFirst();
   int ylast  = fYaxis.GetLast();

   double integral = 0;

   for (Int_t iBinX=xfirst; iBinX<=xlast; ++iBinX)
   {
      for (Int_t iBinY=yfirst; iBinY<=ylast; ++iBinY)
      {
         double binXCenter = fXaxis.GetBinCenter(iBinX);
         double binYCenter = fYaxis.GetBinCenter(iBinY);

         double funcVal = f1.Eval(binXCenter);

         if (binYCenter < funcVal) continue;
         integral += GetBinContent(iBinX, iBinY);
      }
   }

   return integral;
}

}
