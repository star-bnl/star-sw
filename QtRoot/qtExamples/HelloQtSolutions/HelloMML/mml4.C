//  Converted from the original latex4.C ROOT macro 
//  The comment from ROOT macro "latex4.C" follows:
// ------------------------------------------------
// Draw the Greek letters as a table and save the result as GIF, PS, PDF 
// and SVG files. 
// Lowercase Greek letters are obtained by adding a # to the name of the letter. 
// For an uppercase Greek letter, just capitalize the first letter of the
// command name. Some letter have two representations. The name of the
// second one (the "variation") starts with "var".
//Author: Rene Brun
// ------------------------------------------------
//Author: Valeri Fine
void mml4() {
   TCanvas *c2 = new TCanvas("greekmml","greekmml",600,700);

   TLatex l;
   l.SetTextSize(0.03);
   l.SetTextFont(61); // turn ROOT latex engine off
   // Draw the columns titles
   l.SetTextAlign(22);
   l.DrawLatex(0.165, 0.95, "Lower case");
   l.DrawLatex(0.495, 0.95, "Upper case");
   l.DrawLatex(0.825, 0.95, "Variations");

   // Draw the lower case letters
   l.SetTextAlign(12);
   float y, x1, x2;
   y = 0.90; x1 = 0.07; x2 = x1+0.2;
                 l.DrawLatex(x1, y, "alpha : ")   ; l.DrawLatex(x2, y, "<math><mi>&alpha;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "beta : ")    ; l.DrawLatex(x2, y, "<math><mi>&beta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "gamma : ")   ; l.DrawLatex(x2, y, "<math><mi>&gamma;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "delta : ")   ; l.DrawLatex(x2, y, "<math><mi>&delta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "epsilon : ") ; l.DrawLatex(x2, y, "<math><mi>&epsi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "zeta : ")    ; l.DrawLatex(x2, y, "<math><mi>&zeta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "eta : ")     ; l.DrawLatex(x2, y, "<math><mi>&eta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "theta : ")   ; l.DrawLatex(x2, y, "<math><mi>&theta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "iota : ")    ; l.DrawLatex(x2, y, "<math><mi>&iota;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "kappa : ")   ; l.DrawLatex(x2, y, "<math><mi>&kappa;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "lambda : ")  ; l.DrawLatex(x2, y, "<math><mi>&lambda;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "mu : ")      ; l.DrawLatex(x2, y, "<math><mi>&mu;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "nu : ")      ; l.DrawLatex(x2, y, "<math><mi>&nu;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "xi : ")      ; l.DrawLatex(x2, y, "<math><mi>&xi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "omicron : ") ; l.DrawLatex(x2, y, "<math><mi>o</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "pi : ")      ; l.DrawLatex(x2, y, "<math><mi>&pi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "rho : ")     ; l.DrawLatex(x2, y, "<math><mi>&rho;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "sigma : ")   ; l.DrawLatex(x2, y, "<math><mi>&sigma;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "tau : ")     ; l.DrawLatex(x2, y, "<math><mi>&tau;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "upsilon : ") ; l.DrawLatex(x2, y, "<math><mi>&upsilon;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "phi : ")     ; l.DrawLatex(x2, y, "<math><mi>&phi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "chi : ")     ; l.DrawLatex(x2, y, "<math><mi>&chi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "psi : ")     ; l.DrawLatex(x2, y, "<math><mi>&psi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "omega : ")   ; l.DrawLatex(x2, y, "<math><mi>&omega;</mi></math>");

   // Draw the upper case letters
   y = 0.90; x1 = 0.40; x2 = x1+0.2;
                 l.DrawLatex(x1, y, "Alpha : ")   ; l.DrawLatex(x2, y, "<math><mi>A</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Beta : ")    ; l.DrawLatex(x2, y, "<math><mi>B</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Gamma : ")   ; l.DrawLatex(x2, y, "<math><mi>&Gamma;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Delta : ")   ; l.DrawLatex(x2, y, "<math><mi>&Delta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Epsilon : ") ; l.DrawLatex(x2, y, "<math><mi>E</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Zeta : ")    ; l.DrawLatex(x2, y, "<math><mi>Z</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Eta : ")     ; l.DrawLatex(x2, y, "<math><mi>H</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Theta : ")   ; l.DrawLatex(x2, y, "<math><mi>&Theta;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Iota : ")    ; l.DrawLatex(x2, y, "<math><mi>I</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Kappa : ")   ; l.DrawLatex(x2, y, "<math><mi>K</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Lambda : ")  ; l.DrawLatex(x2, y, "<math><mi>&Lambda;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Mu : ")      ; l.DrawLatex(x2, y, "<math><mi>M</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Nu : ")      ; l.DrawLatex(x2, y, "<math><mi>N</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Xi : ")      ; l.DrawLatex(x2, y, "<math><mi>&Xi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Omicron : ") ; l.DrawLatex(x2, y, "<math><mi>O</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Pi : ")      ; l.DrawLatex(x2, y, "<math><mi>&Pi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Rho : ")     ; l.DrawLatex(x2, y, "<math><mi>P</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Sigma : ")   ; l.DrawLatex(x2, y, "<math><mi>&Sigma;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Tau : ")     ; l.DrawLatex(x2, y, "<math><mi>T</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Upsilon : ") ; l.DrawLatex(x2, y, "<math><mi>&Upsilon;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Phi : ")     ; l.DrawLatex(x2, y, "<math><mi>&Phi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Chi : ")     ; l.DrawLatex(x2, y, "<math><mi>X</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Psi : ")     ; l.DrawLatex(x2, y, "<math><mi>&Psi;</mi></math>");
   y -= 0.0375 ; l.DrawLatex(x1, y, "Omega : ")   ; l.DrawLatex(x2, y, "<math><mi>&Omega;</mi></math>");

   // Draw the variations
   x1 = 0.73; x2 = x1+0.2;
   y = 0.7500 ; l.DrawLatex(x1, y, "varepsilon : ") ; l.DrawLatex(x2, y, "<math><mi>&varepsilon;</mi></math>");
   y = 0.6375 ; l.DrawLatex(x1, y, "vartheta : ")   ; l.DrawLatex(x2, y, "<math><mi>vt</mi></math>");
   y = 0.2625 ; l.DrawLatex(x1, y, "varsigma : ")   ; l.DrawLatex(x2, y, "<math><mi>&varsigma;</mi></math>");
   y = 0.1875 ; l.DrawLatex(x1, y, "varUpsilon : ") ; l.DrawLatex(x2, y, "<math><mi>VU</mi></math>");
   y = 0.1500 ; l.DrawLatex(x1, y, "varphi : ")     ; l.DrawLatex(x2, y, "<math><mi>&varphi;</mi></math>");
   y = 0.0375 ; l.DrawLatex(x1, y, "varomega : ")   ; l.DrawLatex(x2, y, "<math><mover><mi>&omega;</mi><mo>&macr;</mo></mover></math>");
}
