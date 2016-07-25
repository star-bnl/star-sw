#include "TAxis.h"
void SetGEANTLabels(TAxis *x) {
  struct Label_t {
    Int_t id;
    const Char_t *name;
  };
  Label_t labels[50] = {
    {      1, "#gamma"},
    {      2, "e^{+}"},
    {      3, "e^{-}"},
    {      4, "#nu"},
    {      5, "#mu^{+}"},
    {      6, "#mu^{-}"},
    {      7, "#pi^{0}"},
    {      8, "#pi^{+}"},
    {      9, "#pi^{-}"},
    {     10, "K^{0}_{L}"},
    {     11, "K^{+}"},
    {     12, "K^{-}"},
    {     13, "n"},
    {     14, "p"},
    {     15, "#bar{p}"},
    {     16, "K^{0}_{S}"},
    {     17, "#eta"},
    {     18, "#Lambda"},
    {     19, "#Sigma^{+}"},
    {     20, "#Sigma^{0}"},
    {     21, "#Sigma^{-}"},
    {     22, "#Xi^{0}"},
    {     23, "#Xi^{-}"},
    {     24, "#Omega^{-}"},
    {     25, "#bar{n}"},
    {     26, "#bar{#Lambda}"},
    {     27, "#bar{#Sigma}^{-}"},
    {     28, "#bar{#Sigma}^{0}"},
    {     29, "#bar{#Sigma}^{+}"},
    {     30, "#bar{#Xi}^{0}"},
    {     31, "#bar{#Xi}^{+}"},
    {     32, "#bar{#Omega}^{+}"},
    {     33, "#tau^{+}"},
    {     34, "#tau^{-}"},
    {     35, "D^{+}"},
    {     36, "D^{-}"},
    {     37, "D^{0}"},
    {     38, "#bar{D^{0}}"},
    {     39, "D^{+}_{S}"},
    {     40, "D^{-}_{S}"},
    {     41, "#Lambda^{+}_{c}"},
    {     42, "W^{+}"},
    {     43, "W^{-}"},
    {     44, "Z^{0}"},
    {     45, "d"},
    {     46, "t"},
    {     47, "#alpha"},
    {     48, "#zeta"},
    {     49, "He^{3}"},
    {     50, "C"}
  };
  x->SetLabelSize(2e-2);
  for (Int_t i = 0; i < 50; i++) {
    x->SetBinLabel(labels[i].id,labels[i].name);
  }
}
