void SetGiDLabels(TAxis *ax = 0) {
  const Char_t *GNames[50] = {
    "#gamma",    "e+",          "e-",    "#nu",      "#mu +", "#mu -",       "#pi 0",       "#pi +",       "#pi -",       "K0L",
    "K+"    ,    "K-",          "n",     "p",        "pbar",  "K0S",         "#eta",        "#Lambda",     "#Sigma +",    "#Sigma 0",
    "#Sigma -",  "#Xi 0",       "#Xi -", "#Omega -", "nbar",  "#Lambda bar", "#Sigma - bar","#Sigma 0 bar","#Sigma + bar","#Xi 0 bar",
    "#Xi + bar", "#Omega + bar","#tau +","#tau -",   "D+",    "D-",          "D0",         ,"D0bar",       "Ds+",         "Ds-",
    "#Lambda c+","W+",          "W-",    "Z0",       "H2",    "H3",          "#alpha",      "g",           "He3",         "C"};
  if (! ax) return;
  Int_t n = ax->GetNbins();
  if (n > 50) n = 50;
  for (Int_t bin = 1; bin <= n; bin++)  ax->SetBinLabel(bin,GNames[bin-1]);
}
