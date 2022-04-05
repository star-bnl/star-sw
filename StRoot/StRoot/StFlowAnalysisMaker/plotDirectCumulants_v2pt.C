///////////////////////////////////////////////////////////////////////////////
//
// $Id: plotDirectCumulants_v2pt.C,v 1.1 2010/03/08 16:54:53 posk Exp $
//
// Authors:      Dhevan Gangadharan and Art Poskanzer, Jan 2010
//
// Description:  Macro to replot the v2(pt) TGraphErrors made by directCumulants_v2.C
//               cent=1 is the most central
//               cent=0 is the yield weighted average of all the others.
//
////////////////////////////////////////////////////////////////////////////////////////////

#include <math.h> 
#include "TMath.h" 
#include <iostream.h>
#include <iomanip.h>

plotDirectCumulants_v2pt() {

  TCanvas* cOld = (TCanvas*)gROOT->GetListOfCanvases(); // delete old canvas
  if (cOld) cOld->Delete();
  
  bool ptGraphs = kTRUE;

  gROOT->SetStyle("Bold");                              // set style
  gStyle->SetOptStat(0);

  const int CENTBINS=9;
  const int PTBINS= 61; // max ptbins
  float v2max = 0.4;
  
  TFile graphFile("flow.dirCumulant.graphs.root", "READ");
  //graphFile.ls();

  TGraphErrors* charged_v2_2[CENTBINS+1];
  TGraphErrors* charged_v2_4[CENTBINS+1];
  TCanvas* can[CENTBINS+1];
  int canvasWidth = 780, canvasHeight = 600;             // landscape
  for (int cent=0; cent<CENTBINS+1; cent++) {
    if (ptGraphs) {
      TString* canName = new TString("centrality_"); 
      *canName += cent; 
      cout << "plot " << canName->Data() << endl;
      can[cent] = new TCanvas(canName->Data(), canName->Data(), canvasWidth, canvasHeight);
      TLegend *legend = new TLegend(.17,.67,.5,.87,NULL,"brNDC");
      legend->SetFillColor(kWhite);
    }

    // v22
    TString graphName("v22_"); 
    graphName += cent; 
    charged_v2_2[cent] = dynamic_cast<TGraphErrors*>(graphFile.Get(graphName.Data()));
    if (!charged_v2_2[cent]) {
      cout << "### Can't find graph " << graphName.Data() << endl;
      return;
    }
    charged_v2_2[cent]->SetMarkerStyle(20);
    charged_v2_2[cent]->SetMarkerColor(2);
    charged_v2_2[cent]->SetLineColor(2);
    charged_v2_2[cent]->SetMinimum(0.);
    charged_v2_2[cent]->SetMaximum(v2max);
    charged_v2_2[cent]->GetXaxis()->SetTitle("P_{t} (GeV/c)");
    charged_v2_2[cent]->GetYaxis()->SetTitle("v_{2}");
    
    // v24
    TString graphName("v24_"); 
    graphName += (cent); 
    charged_v2_4[cent] = dynamic_cast<TGraphErrors*>(graphFile.Get(graphName.Data()));
    if (!charged_v2_4[cent]) {
      cout << "### Can't find graph " << graphName.Data() << endl;
      return;
    }
    charged_v2_4[cent]->SetMarkerStyle(20);
    charged_v2_4[cent]->SetMarkerColor(4);
    charged_v2_4[cent]->SetLineColor(4);
    charged_v2_4[cent]->SetMinimum(0.);
    charged_v2_4[cent]->SetMaximum(v2max);
    charged_v2_4[cent]->GetXaxis()->SetTitle("P_{t} (GeV/c)");
    charged_v2_4[cent]->GetYaxis()->SetTitle("v_{2}{4}");
    
    if (ptGraphs) {
      charged_v2_2[cent]->Draw("AP");
      legend->AddEntry(charged_v2_2[cent],"charged hadron v_{2}{2}","p");
      charged_v2_4[cent]->Draw("P");
      legend->AddEntry(charged_v2_4[cent],"charged hadron v_{2}{4}","p");
      
      legend->Draw("same");
    }

    //can[cent]->Print(".pdf");
    
  }//cent

  // multi graph
  int canvasWidth = 600, canvasHeight = 780;             // portrait
  TString* canName = new TString("v2_pt");
  cout << "plot " << canName->Data() << endl;
  TCanvas* canMulti = new TCanvas(canName->Data(), canName->Data(), canvasWidth, canvasHeight);
  canMulti->ToggleEventStatus();
  TPaveLabel* canTitle = new TPaveLabel(0.1,0.96,0.9,0.99,canName->Data());
  canTitle->Draw();
  TLegend *legendMulti = new TLegend(.17,.67,.5,.87,NULL,"brNDC");
  legendMulti->SetFillColor(kWhite);
  gStyle->SetLabelSize(0.1,"x");
  TPad* graphPad = new TPad("Graphs","Graphs",0.01,0.05,0.97,0.95);
  graphPad->Draw();
  graphPad->cd();
  int columns = 2;
  int rows = (CENTBINS+1)/2;
  graphPad->Divide(columns,rows);

  for (int cent=0; cent<CENTBINS+1; cent++) {
    TString graphName("cent_"); 
    graphName += cent; 
    graphPad->cd(cent+1);
    charged_v2_2[cent]->SetTitle(graphName);
    charged_v2_2[cent]->Draw("AP");
    charged_v2_4[cent]->Draw("P");
    if (cent==0) {
      legendMulti->AddEntry(charged_v2_2[cent],"charged hadron v_{2}{2}","p");
      legendMulti->AddEntry(charged_v2_4[cent],"charged hadron v_{2}{4}","p");
      legendMulti->Draw("same");
    }
  }//cent
  graphPad->cd(0);

  graphFile.Close();
  
}
