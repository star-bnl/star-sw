// The original macro latex2.C draws 4 Latex-style formula in a canvas and prints the canvas
// as a Postscript file.
// Note that this macro relies on a first implementation of the TLatex class.
// There are still some discrepancies between the result on the screen
// and the result on Postscript.
//Author: Rene Brun
//-----------------------
// This  macro doens't create any Postscript file (yet) and uses the external latex2.mml
// file
//Author: Valeri Fine
//-----------------------
void mml2(){
   TCanvas *c1 = new TCanvas("c1");
   TLatex l;
	l.SetTextFont(61); // turn off latex (mis)aligment
   l.SetTextAlign(23);
   l.SetTextSize(0.1);
   l.DrawLatex(0.5,0.3,"@latex2.mml");
}
