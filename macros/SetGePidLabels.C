/* 
    Set axis label to GEANT particle names
*/
#include "TAxis.h"
#include "TMath.h"
//________________________________________________________________________________
void SetGePidLabels(TAxis *x) {
  if (! x) return;
  const Char_t *names[51] = {"",//           1                  2                 3                  4                5 
			     "#gamma"         ,           "e^{+}",          "e^{-}",             "#nu",       "#mu^{+}",  			
			     "#mu^{-}"        ,         "#pi^{0}",        "#pi^{+}",          "#pi^{-}",    "K^{0}_{L}",			
			     "K^{+}"          ,           "K^{-}",               "n",               "p",      "#bar{p}",			
			     "K^{0}_{S}"      ,            "#eta",         "#Lambda",      "#Sigma^{+}",   "#Sigma^{0}",		
			     "#Sigma^{-}"     ,         "#Xi^{0}",         "#Xi^{-}",      "#Omega^{-}",      "#bar{n}",			
			     "#bar{#Lambda}"  ,"#bar{#Sigma}^{-}","#bar{#Sigma}^{0}","#bar{#Sigma}^{+}","#bar{#Xi}^{0}",	
			     "#bar{#Xi}^{+}"  ,"#bar{#Omega}^{+}",        "#tau^{+}",        "#tau^{-}",        "D^{+}",                    
			     "D^{-}"          ,           "D^{0}",     "#bar{D}^{0}",       "D_{s}^{+}",    "D_{s}^{-}",
			     "#Lambda_{c}^{+}",           "W^{+}",           "W^{-}",           "Z^{0}",            "d",
			     "t"              ,          "#alpha",               "g",          "He^{3}",     "#hat{C}"};
  Int_t n = x->GetNbins();
  for (Int_t i = 0; i < n; i++) {
    Int_t  j = TMath::Nint(x->GetBinCenter(i+1));
    if (j >= 0 && j <= 50) {
      x->SetBinLabel(i+1,names[j]);
    }
  }
}
//void SetGePidLabels(Int_t *x) {return SetGePidLabels((TAxis *)x);}
