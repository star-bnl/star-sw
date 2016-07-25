#include "TDatime.h"
#include "TH1.h"
#include "TRandom.h"
void htime() {
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*
//*-*  This program creates :
//*-*    - a one dimensional histogram with X== time
//*-*  These objects are filled with some random numbers and saved on a file.
//*-*
//*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

    //  gROOT->Reset();

// Create a new canvas.
//   c1 = new TCanvas("c1","Time Xaxis histogram",200,10,700,500);
//   c1->SetFillColor(42);
//   c1->GetFrame()->SetFillColor(21);
//   c1->GetFrame()->SetBorderSize(6);
//   c1->GetFrame()->SetBorderMode(-1);

// Create a new ROOT binary machine independent file.
// Note that this file may contain any kind of ROOT objects, histograms,
// pictures, graphics objects, detector geometries, tracks, events, etc..
// This file is now becoming the current directory.

  TDatime da(19950101,0);//0,0,0,0,0,0);
  UInt_t  uoffset = da.Convert();
  TDatime tdBeg(20140901,0);
  UInt_t  uBeg = tdBeg.Convert() - uoffset;
  printf ("begTime %ud = %s\n",uBeg,tdBeg.AsString());

  TDatime tdEnd(20141114,0);
  UInt_t  uEnd = tdEnd.Convert() - uoffset;
  printf ("endTime %ud = %s\n",uEnd,tdEnd.AsString());

  int nBins = (uEnd-uBeg)/(24*60*60);




// Create some histograms, a profile histogram and an ntuple
  TH1F *hpx    = new TH1F("hpx","This is the time distribution",nBins,uBeg,uEnd);
  //  hpx->GetXaxis()->SetTimeDisplay(1);
//   hpx->GetXaxis()->SetTimeOffset(da.Convert(),"gmt");
//  hpx->GetXaxis()->LabelsOption("v");

//  Set canvas/frame attributes (save old attributes)
  hpx->SetFillColor(48);


// Fill histograms randomly
  gRandom->SetSeed();
  const Int_t kUPDATE = 1000;
  for (Int_t i = 0; i < 25000; i++) {
     double r = gRandom->Rndm();
     UInt_t u = uBeg + r*(uEnd - uBeg);
     hpx->Fill(u);
//      if (i && (i%kUPDATE) == 0) {
//         if (i == kUPDATE) hpx->Draw();
//         c1->Modified();
//         c1->Update();
//         if (gSystem->ProcessEvents())
//            break;
//      }
  }
  hpx->Draw();
  hpx->GetXaxis()->SetTimeDisplay(1);
  //  hpx->GetXaxis()->SetTimeOffset(uoffset);
  //  hpx->GetXaxis()->SetTimeFormat("%m\\/%d\\/%y");
}
