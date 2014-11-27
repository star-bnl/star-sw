// This macro draws 4 Latex-style formula in a canvas and prints the canvas
// as a Postscript file.
// Note that this macro relies on a first implementation of the TLatex class.
// There are still some discrepancies between the result on the screen
// and the result on Postscript.
// Also some differences will be seen between Unix servers depending
// the version of X in use. Results on WindowsNT are also slightly different.
//Author: Rene Brun
void mml() {
   TCanvas *c1 = new TCanvas("c1","test",600,700);

   // write formulas
   TLatex l;
   
	l.SetTextFont(61); //!!!  turn the latex redndereing off

   l.SetTextAlign(12);
   l.SetTextSize(0.04);
   l.DrawLatex(0.1,0.9,"@latex.1.mml");
   l.DrawLatex(0.1,0.7,"@latex.2.mml");
   l.DrawLatex(0.1,0.5,"@latex.3.mml");
   l.DrawLatex(0.1,0.3,"@latex.4.mml");
   l.DrawLatex(0.1,0.1,"@latex.5.mml");
//   c1->Print("latex.ps");
}
