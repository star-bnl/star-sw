//*CMZ :          10/06/99  16.26.07  by  Valery Fine(fine@bnl.gov)
//*-- Author :    Valery Fine     10/06/99
// Copyright (C) Valery Fine, Brookhaven National Laboratory, 1999. All right reserved
// $Id: basic3dPrimitives.C,v 1.1 1999/06/10 19:02:53 fine Exp $
// $Log: basic3dPrimitives.C,v $
// Revision 1.1  1999/06/10 19:02:53  fine
// New macro to show the basic fearture of new 3D classes
//
{

//
// To see the output of this macro, click begin_html <a href="gif/basic3dPrimitives.gif">here</a> end_html
//
   gROOT->Reset();
//_____________________________
//
//  Load share library
//_____________________________
   gSystem->Load("St_base");
//_____________________________
//
// create a canvas and a pad
//_____________________________
   c1 = new TCanvas("c1","PolyLine3D & PolyMarker3D Window",200,10,700,500);
   p1 = new TPad("p1","p1",0.05,0.02,0.95,0.82,46,3,1);
   p1->Draw();
   p1->cd();

//_____________________________
//
// create a first PolyLine3D
//_____________________________
   St_PolyLine3D pl3d1(5);
   Int_t i;

   // set points
   Float_t *pp = pl3d1.GetP();
   pl3d1.SetPoint(0, 10, 10, 10);
   for(i=0;i<=pl3d1.GetLastPosition();i++) printf(" %d: x=%f y=%f z=%f\n",i, pp[3*i], pp[3*i+1], pp[3*i+2]);
   printf(" 1 -- \n");

   pl3d1.SetPoint(1, 15.0, 15.0, 10.0);
   for(i=0;i<=pl3d1.GetLastPosition();i++) printf(" %d: x=%f y=%f z=%f\n",i, pp[3*i], pp[3*i+1], pp[3*i+2]);
   printf(" 2 -- \n");

   pl3d1.SetPoint(2, 20, 15, 15);
   for(i=0;i<=pl3d1.GetLastPosition();i++) printf(" %d: x=%f y=%f z=%f\n",i, pp[3*i], pp[3*i+1], pp[3*i+2]);
   printf(" 3 -- \n");

   pl3d1.SetPoint(3, 20, 20, 20);
   for(i=0;i<=pl3d1.GetLastPosition();i++) printf(" %d: x=%f y=%f z=%f\n",i, pp[3*i], pp[3*i+1], pp[3*i+2]);
   printf(" 4 -- \n");

   pl3d1.SetPoint(4, 10, 10, 20);
   for(i=0;i<=pl3d1.GetLastPosition();i++) printf(" %d: x=%f y=%f z=%f\n",i, pp[3*i], pp[3*i+1], pp[3*i+2]);
   printf(" 5 -- \n");

   // set attributes
   pl3d1.SetVisibility(1);
   pl3d1.SetSizeAttribute(3);
   pl3d1.SetColorAttribute(5);
   
 
//_____________________________
//
// create a second PolyLine3D
//_____________________________
   St_PolyLine3D pl3d2(4);

   // set points
   pl3d2.SetPoint(0, 5, 10, 5);
   pl3d2.SetPoint(1, 10, 15, 8);
   pl3d2.SetPoint(2, 15, 15, 18);
   pl3d2.SetPoint(3, 5, 20, 20);
   pl3d2.SetPoint(4, 10, 10, 5);

   // set attributes
   pl3d2.SetSizeAttribute(5);
   pl3d2.SetColorAttribute(2);
   pl3d2.SetVisibility(1);
//_____________________________
//
// create a first PolyMarker3D
//_____________________________
   St_PolyLine3D pm3d1(12,"P");

   // set points
   pm3d1.SetPoint(0, 10, 10, 10);
   pm3d1.SetPoint(1, 11, 15, 11);
   pm3d1.SetPoint(2, 12, 15, 9);
   pm3d1.SetPoint(3, 13, 17, 20);
   pm3d1.SetPoint(4, 14, 16, 15);
   pm3d1.SetPoint(5, 15, 20, 15);
   pm3d1.SetPoint(6, 16, 18, 10);
   pm3d1.SetPoint(7, 17, 15, 10);
   pm3d1.SetPoint(8, 18, 22, 15);
   pm3d1.SetPoint(9, 19, 28, 25);
   pm3d1.SetPoint(10, 20, 12, 15);
   pm3d1.SetPoint(11, 21, 12, 15);

   // set marker size, color & style
   pm3d1.SetSizeAttribute(2);
   pm3d1.SetColorAttribute(4);
   pm3d1.SetStyleAttribute(2);
   pm3d1.SetVisibility(1);
//_____________________________
//
// create a second PolyMarker3D
//_____________________________
   St_PolyLine3D pm3d2(8,"P");

   pm3d2.SetPoint(0, 22, 15, 15);
   pm3d2.SetPoint(1, 23, 18, 21);
   pm3d2.SetPoint(2, 24, 26, 13);
   pm3d2.SetPoint(3, 25, 17, 15);
   pm3d2.SetPoint(4, 26, 20, 15);
   pm3d2.SetPoint(5, 27, 15, 18);
   pm3d2.SetPoint(6, 28, 20, 10);
   pm3d2.SetPoint(7, 29, 20, 20);

   // set marker size, color & style
   pm3d2.SetSizeAttribute(2);
   pm3d2.SetColorAttribute(1);
   pm3d2.SetStyleAttribute(8);
   pm3d2.SetVisibility(1);

//_____________________________
//
// creating a view
//_____________________________
   view = new TView(1);
   view->SetRange(5,5,5,25,25,25);
//_____________________________
//
// draw
//_____________________________
   pl3d1.Draw("same");
   pl3d2.Draw("same");
   pm3d1.Draw("same");
   pm3d2.Draw("same");
//_____________________________
//
//   Draw axice
//_____________________________
    St_PolyLine3D::Axis();
   //
   // draw a title/explanation in the canvas pad
   c1->cd();
   TPaveText title(0.1,0.85,0.9,0.97);
   title.SetFillColor(24);
   title.AddText("Examples of 3-D primitives");
   TText *click=title.AddText("Click anywhere on the picture to rotate");
   click.SetTextColor(4);
   title.Draw();
   c1->Modified();
   c1->Update();     
   p1->cd();
//   p1->x3d();
}
