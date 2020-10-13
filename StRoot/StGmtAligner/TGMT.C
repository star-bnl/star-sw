/*
  root.exe 'bfc.C(-1,"gmtAligner,tpcDb,corrY,analysis,mysql,nodefault")' 'Chain.C+("*.root","T")' 'TGMT.C+(tChain)'
 */
#define TGMT_xxx
#include <assert.h>
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TRVector.h"
#include "TRMatrix.h"
#include "TRSymMatrix.h"
#include "StRoot/StGmtAligner/TGMT.h"
#include "Riostream.h"
ClassImp(TT);
struct Geometry_t {
  Int_t Layer;
  Int_t Sector;
  Int_t NoLadders;
  Int_t NoWafers;
};
const Int_t NoLayers = 4;
 // Layer  Sector  ladder wafer 
const Geometry_t HftConfig[] = 
  {    {1,  10,    10,   10}, // 
       { 2,  10,    30,   10}, //
       { 3,   -1,    24,   12}, // 
       { 4,   -1,    20,   16}, // 
  };
const Int_t BL[4] = {10, 30, 24, 20}; // ladders in layer
struct HybridFit_t {
  HybridFit_t() {noentries=0; AmX = TRVector(6); S = TRSymMatrix(6);}
  Int_t noentries;
  TRVector AmX;
  TRSymMatrix S;
};
//________________________________________________________________________________
void TGMT(TChain *tChain) {
  TT t(tChain);
  t.Loop();
}
