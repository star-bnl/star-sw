#include <iostream>
#include <iomanip>
#include <cmath>

#include <TCanvas.h>
#include <TAxis.h> 

#include "PlottingEngineRoot.hh"

namespace Garfield {

namespace PlotSpace {

  Medium* medium;
  
  double ElectronVelocityFunction(double* pos, double* par) {
      
    double vx, vy, vz;
    if (!medium->ElectronVelocity(pos[0], 0., 0., 0., 0., 0., vx, vy, vz)) {
      return 0.;
    }
    return sqrt(vx * vx + vy * vy + vz * vz);

  }
  
  double HoleVelocityFunction(double* pos, double* par) {
      
    double vx, vy, vz;
    if (!medium->HoleVelocity(pos[0], 0., 0., 0., 0., 0., vx, vy, vz)) {
      return 0.;
    }
    return sqrt(vx * vx + vy * vy + vz * vz);

  }
  
  double IonVelocityFunction(double* pos, double* par) {
  
    double vx, vy, vz;
    if (!medium->IonVelocity(pos[0], 0., 0., 0., 0., 0., vx, vy, vz)) {
      return 0.;
    }
    return sqrt(vx * vx + vy * vy + vz * vz);
    
  }

  double ElectronTownsendFunction(double* pos, double* par) {
      
    double alpha;
    if (!medium->ElectronTownsend(pos[0], 0., 0., 0., 0., 0., alpha)) {
      return 0.;
    }
    return alpha;

  }
  
  double HoleTownsendFunction(double* pos, double* par) {
      
    double alpha;
    if (!medium->HoleTownsend(pos[0], 0., 0., 0., 0., 0., alpha)) {
      return 0.;
    }
    return alpha;

  }
  
  double ElectronAttachmentFunction(double* pos, double* par) {
      
    double eta;
    if (!medium->ElectronAttachment(pos[0], 0., 0., 0., 0., 0., eta)) {
      return 0.;
    }
    return eta;

  }
  
  double HoleAttachmentFunction(double* pos, double* par) {
      
    double eta;
    if (!medium->HoleAttachment(pos[0], 0., 0., 0., 0., 0., eta)) {
      return 0.;
    }
    return eta;

  }  
  
}


PlottingEngineRoot::PlottingEngineRoot() {

  garfieldStyle = new TStyle("Garfield","Garfield Style");
  SetDefaultStyle();
 
  f1 = new TF1("f1", "sin(x)", 0., 1.);
  f2 = new TF1("f2", "cos(x)", 0., 1.);
   
}

PlottingEngineRoot::~PlottingEngineRoot() {

  delete garfieldStyle;
  
}

void 
PlottingEngineRoot::PlotVelocity(Medium* m, 
                                 const bool electron, 
                                 const bool hole,
                                 const bool ion) {

  if (m == 0) {
    std::cerr << "PlottingEngineRoot::PlotVelocity:" << std::endl;
    std::cerr << "    Medium is not defined." << std::endl;
    return;
  }
  
  PlotSpace::medium = m;
  if (electron) {  
    if (f1 != 0) {
      delete f1;
      f1 = 0;
    }
    f1 = new TF1("fe", PlotSpace::ElectronVelocityFunction, xMin, xMax, 0);
    f1->GetXaxis()->SetTitle(xLabel.c_str());
    f1->GetYaxis()->SetTitle(yLabel.c_str());
    f1->SetTitle(title.c_str());
    f1->SetLineColor(GetRootColor(color1));
    f1->Draw("");
  }
  
  if (hole) {
    if (f2 != 0) {
      delete f2;
      f2 = 0;
    }
    f2 = new TF1("fh", PlotSpace::HoleVelocityFunction, xMin, xMax, 0);
    f2->GetXaxis()->SetTitle(xLabel.c_str());
    f2->GetYaxis()->SetTitle(yLabel.c_str());
    f2->SetTitle(title.c_str());
    f2->SetLineColor(GetRootColor(color2));
    f2->Draw("same");
  } else if (ion) {
    if (f2 != 0) {
      delete f2;
      f2 = 0;
    }
    f2 = new TF1("fi", PlotSpace::IonVelocityFunction, xMin, xMax, 0);
    f2->GetXaxis()->SetTitle(xLabel.c_str());
    f2->GetYaxis()->SetTitle(yLabel.c_str());
    f2->SetTitle(title.c_str());
    f2->SetLineColor(GetRootColor(color2));
    f2->Draw("same");
  }

}

void 
PlottingEngineRoot::PlotTownsend(Medium* m, 
                     const bool electron, const bool hole) {

  if (m == 0) {
    std::cerr << "PlottingEngineRoot::PlotTownsend:" << std::endl;
    std::cerr << "    Medium is not defined." << std::endl;
    return;
  }

  PlotSpace::medium = m;
  if (electron) {
    if (f1 != 0) {
      delete f1;
      f1 = 0;
    }
    f1 = new TF1("fe", PlotSpace::ElectronTownsendFunction, xMin, xMax, 0);
    f1->GetXaxis()->SetTitle(xLabel.c_str());
    f1->GetYaxis()->SetTitle(yLabel.c_str());
    f1->SetTitle(title.c_str());
    f1->SetLineColor(GetRootColor(color1));
    f1->Draw("");
  }
  
  if (hole) {
    if (f2 != 0) {
      delete f2;
      f2 = 0;
    }
    f2 = new TF1("fh", PlotSpace::HoleTownsendFunction, xMin, xMax, 0);
    f2->GetXaxis()->SetTitle(xLabel.c_str());
    f2->GetYaxis()->SetTitle(yLabel.c_str());
    f2->SetTitle(title.c_str());
    f2->SetLineColor(GetRootColor(color2));
    f2->Draw("same");
  }

}

void 
PlottingEngineRoot::PlotAttachment(Medium* m, 
                     const bool electron, const bool hole) {

  if (m == 0) {
    std::cerr << "PlottingEngineRoot::PlotAttachment:" << std::endl;
    std::cerr << "    Medium is not defined." << std::endl;
    return;
  }

  PlotSpace::medium = m;
  if (electron) {
    if (f1 != 0) {
      delete f1;
      f1 = 0;
    }
    f1 = new TF1("fe", PlotSpace::ElectronAttachmentFunction, xMin, xMax, 0);
    f1->GetXaxis()->SetTitle(xLabel.c_str());
    f1->GetYaxis()->SetTitle(yLabel.c_str());
    f1->SetTitle(title.c_str());
    f1->SetLineColor(GetRootColor(color1));
    f1->Draw("");
  }
  
  if (hole) {
    if (f2 != 0) {
      delete f2;
      f2 = 0;
    }
    f2 = new TF1("fh", PlotSpace::HoleAttachmentFunction, xMin, xMax, 0);
    f2->GetXaxis()->SetTitle(xLabel.c_str());
    f2->GetYaxis()->SetTitle(yLabel.c_str());
    f2->SetTitle(title.c_str());
    f2->SetLineColor(GetRootColor(color2));
    f2->Draw("same");
  }

}

void 
PlottingEngineRoot::PlotSignal(Sensor* s, const std::string label) {

  if (s == 0) {
    std::cerr << "PlottingEngineRoot::PlotSignal:" << std::endl;
    std::cerr << "    Sensor is not defined." << std::endl;
    return;
  }
  
  int nBins;
  double t0;
  double dt;
  s->GetTimeWindow(t0, dt, nBins);
    
  if (hSignal != 0) {
    delete hSignal;
    hSignal = 0;
  }
  hSignal = new TH1F("hSignal", label.c_str(), nBins, t0, t0 + nBins * dt);
  hSignal->GetXaxis()->SetTitle("time [ns]");
  hSignal->GetYaxis()->SetTitle("signal");
  
  double sig = 0.;  
  for (int i = nBins; i--;) {
    sig = s->GetSignal(label, i);
    hSignal->SetBinContent(i, sig);
  }
  hSignal->Draw();
  
}

void 
PlottingEngineRoot::SetDefaultStyle() {

  garfieldStyle->Reset();  
  garfieldStyle->SetCanvasBorderMode(0);
  garfieldStyle->SetCanvasColor(0);
  garfieldStyle->SetPadBorderMode(0);
  garfieldStyle->SetPadColor(0);
  garfieldStyle->SetFrameBorderMode(0);
  garfieldStyle->SetTitleBorderSize(0);
  garfieldStyle->SetTitleColor(1, "xyz");
  garfieldStyle->SetTitleColor(1, "t");
  garfieldStyle->SetTitleFillColor(0);
  garfieldStyle->SetTitleFont(132, "xyz");
  garfieldStyle->SetTitleFont(132, "t");
  garfieldStyle->SetStatBorderSize(0);    
  garfieldStyle->SetStatColor(0);
  garfieldStyle->SetStatFont(132);
  garfieldStyle->SetStatFontSize(0.025);
  garfieldStyle->SetStatX(0.88);
  garfieldStyle->SetStatY(0.88);
  garfieldStyle->SetStatW(0.25);
  garfieldStyle->SetStatH(0.10);
  garfieldStyle->SetOptStat(111110);
  garfieldStyle->SetLabelFont(132, "xyz");
  garfieldStyle->SetPaperSize(26, 20);
  garfieldStyle->SetFuncWidth(2);
  garfieldStyle->SetHistLineColor(kOrange);
  
  garfieldStyle->cd();  
    
}

int 
PlottingEngineRoot::GetRootColor(std::string color) {

  // Convert to upper-case
  for (unsigned int i = 0; i < color.length(); ++i) {
    color[i] = toupper(color[i]);
  }

  if (color == "ORANGE") {
    return kOrange;
  } else if (color == "DARK-GREEN") {
    return kGreen + 2;
  } else {
    return kBlack;
  }

}

}
