//! \file Draw3D.C 
/*!
  \brief Draw3D.C macro is to test STAR EventDisplay facility
  \Author  Valeri Fine ( fine@bnl.gov )
 */
void Draw3D()
{
   gROOT->Macro("Load.C");  //< Load STAR framework shared libraries
   gEventDisplay->Draw3DTest(); //< Invoke the built-in rendering test
   gEventDisplay->Print("Draw3DTest.wrl"); //< Save the 3D scene into the file
}
