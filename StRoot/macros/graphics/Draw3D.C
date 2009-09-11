//! \file Draw3D.C 
/*!
  \brief Draw3D.CE macro is to test STAR EventDisplay facility
 */
void Draw3D()
{
   gROOT->Macro("Load.C");  //< Load STAR framework shared libraries
   gEventDisplay->Draw3DTest(); //< Inloke the built-in rednering test
   gEventDisplay->Print("Draw3DTest.wrl"); //< Save the 3D scene into the file
}
