// $Id: TestStarPalette.C,v 1.2 1999/06/25 19:17:27 kathy Exp $
// $Log: TestStarPalette.C,v $
// Revision 1.2  1999/06/25 19:17:27  kathy
// fix the Palette macros so that you can run StarPalette directly from TestStarPalette - thanks, Thoams & Gene!
//
// Revision 1.1  1999/06/24 20:42:22  kathy
//  test to show results of Jon Gans' macro to setup star color palette
//
//
//======================================================================
// owner: Jon Gans
// what it does: 
//      Example to show how what color scheme will be set up when
//      using StarPalette.C
//
// To use:
//    Run root4star, then
//      .x TestStarPalette.C
//=======================================================================


{
// to run another macro from this macro:
  gROOT->LoadMacro("StarPalette.C");
  StarPalette();

  gROOT->Reset();  
 
  TCanvas *c1 = new TCanvas("c1","Spectrum Palette",200,10,900,500);
  c1->Divide(2,1); 
  
  // Generate a 2-D function
  TF2 *f2 = new TF2("f2","exp( -5.*(x^2+y^2)^(1/2))*cos((x^2+y^2)^(1/2)*10.0) ",-1.0,1.0,-1.0,1.0); // Example graph

  //The following calls are important to set up the color scheme for the 'f2' calls.
 
  f2->SetContour(48);   // This is needed to use all 50 colors. (If above 48 it will cycle through color map).
  f2->SetFillColor(0);  // If using a histogram it is not nessesary.
  
  c1->cd(1);
  f2->Draw("SURF1");

  c1->cd(2);
  f2->Draw("COLZ");
  
 
}



