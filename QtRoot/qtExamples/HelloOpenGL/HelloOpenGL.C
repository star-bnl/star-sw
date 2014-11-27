void HelloOpenGL() 
{
   gStyle->SetPalette(1);
   gSystem->Load("libHelloOpenGL");
   TF2 *f = new TF2("f","x*sin(x)*y*sin(y)",-4,4,-4,4);
   f->SetNpx(80);
   f->SetNpy(80);
   f->SetFillColor(kGreen);
//  Use the regular TCanvas to draw the TF2   
  // f->Draw("colz");
   
//  Use the QGLViewer  to draw the TF2  (single color)
   RootViewer *v = new RootViewer(f);
   v->Show();

   RootViewer *v = new RootViewer(f);
   v->SetColZ();
   v->SetTop();
   v->Show();
#if 0
//  Use the QGLViewer  to draw the TF2  (colz option)
   v = new RootViewer(f);
   v->SetColZ();
   v->Show();

//  Change the mesh points and redraw the function.
   TF2 *f1 = new TF2("f1","0.1*x*x+0.1*y*y",-10,10,-10,10);
   f1->SetNpx(60);
   f1->SetNpy(60);
   v = new RootViewer(f1);
   v->SetColZ();
   v->Show();
   
#endif
}
