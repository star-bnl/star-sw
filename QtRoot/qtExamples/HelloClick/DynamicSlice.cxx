#include "HelloClick.h"
#include "TCanvas.h"
#include "TH2F.h"
#include "TF1.h"
#include "TRandom.h"
#include "TList.h"
#include "TQtWidget.h"

 // Show the slice of a TH2 following the mouse position
void HelloClick::DynamicSlice()
{
   // Create a new canvas.
   TCanvas *c1 = fDynamicSliceWidget->GetCanvas();
   c1->SetName("c1");
   c1->SetTitle("Dynamic Slice Example");
   c1->SetFillColor(42);
   c1->SetFrameFillColor(33);
  
   //create a 2-d histogram, fill and draw it
   TH2F *hpxpy  = new TH2F("hpxpy","py vs px",40,-4,4,40,-4,4);
   hpxpy->SetStats(0);
   Double_t px,py;
   for (Int_t i = 0; i < 50000; i++) {
      gRandom->Rannor(px,py);
      hpxpy->Fill(px,py);
   }
   c1->GetListOfPrimitives()->Add(hpxpy,"col");
   c1->Modified();
}

void HelloClick::DynamicExec(TObject *select, unsigned int /*event*/, TCanvas *c1)
{
   // Example of function called when a mouse event occurs in a pad.
   // When moving the mouse in the canvas, a second canvas shows the
   // projection along X of the bin corresponding to the Y position
   // of the mouse. The resulting histogram is fitted with a gaussian.

   // This more elaborated example can be used as a starting point
   // to develop more powerful interactive applications exploiting CINT
   // as a development engine.
   //
   // Author:  Rene Brun (modified by Valeri Fine for Qt applications)
   
//    TObject *select = gPad->GetSelected();
   if (select  && select->InheritsFrom("TH2")) {
      TH2 *h = (TH2*)select;

      int py = c1->GetEventY();
      c1->SetUniqueID(py);

      Float_t upy = c1->AbsPixeltoY(py);
      Float_t y = c1->PadtoY(upy);

      //Activate the TCanvas c2 from the bottom frame 
      TVirtualPad *padsav = gPad;
      TCanvas *c2 = fDynamicExecWidget->GetCanvas();
      delete c2->GetPrimitive("Projection");
      c2->SetGrid();
      c2->cd();

      //draw slice corresponding to mouse position
      Int_t biny = h->GetYaxis()->FindBin(y);
      TH1D *hp = h->ProjectionX("",biny,biny);
      hp->SetFillColor(38);
      char title[80];
      sprintf(title,"Projection of biny=%d",biny);
      hp->SetName("Projection");
      hp->SetTitle(title);
      hp->Fit("gaus","ql");
      hp->GetFunction("gaus")->SetLineColor(kRed);
      hp->GetFunction("gaus")->SetLineWidth(6);
      fDynamicExecWidget->Refresh();
      padsav->cd();
   } else {
      c1->SetUniqueID(0);
   }
}
