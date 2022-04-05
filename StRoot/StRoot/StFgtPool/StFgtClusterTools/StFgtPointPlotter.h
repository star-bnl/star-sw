#ifndef _ST_FGT_POINT_PLOTTER_
#define _ST_FGT_POINT_PLOTTER_
#include "StMaker.h"


#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TF1.h>
class StFgtPointCollection;
class StFgtPoint;
class StFgtPointPlotter : public StMaker
{
 public:
  virtual ~StFgtPointPlotter();
  StFgtPointPlotter(const Char_t* name="FgtPointPlotter");
   Int_t Init();
   Int_t Make();
   Int_t Finish();
 protected:

   TFile* outRootFile;
   TH2D** histos;
   TH2D* tbCorrelation;
   TH1D** tbHistos;


 private:
   ClassDef(StFgtPointPlotter,1);
};
#endif
