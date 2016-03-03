#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "StEmcPool/StBEMCPlots/BEMCPlots.h"
//#include "StEmcPool/StBEMCPlots/BEMCPlotsPresenter.h"
#include "StEmcPool/StBEMCPlots/BEMCPlotsNames.h"
#include "StEmcUtil/database/StEmcDecoder.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TLine.h>
#include <TLatex.h>
#include <TArrow.h>

#include <math.h>
#include "bemcBuilder.h"
#include <RTS/include/rtsLog.h>

ClassImp(bemcBuilder);

#define min(A, B) (((A) < (B)) ? (A) : (B))
#define max(A, B) (((A) > (B)) ? (A) : (B))

Int_t bemcBuilder::linesColor = 16;
Int_t bemcBuilder::crateMinSoftId[] = {4661, 2421, 2581, 2741, 2901, 3061, 3221, 3381, 3541, 3701, 3861, 4021, 4181, 4341, 4501, 2181, 2021, 1861, 1701, 1541, 1381, 1221, 1061,  901,  741,  581,  421,  261,  101,    1};

Int_t bemcBuilder::crateMaxSoftId[] = {4800, 2580, 2740, 2900, 3060, 3220, 3380, 3540, 3700, 3860, 4020, 4180, 4340, 4500, 4660, 2340, 2180, 2020, 1860, 1700, 1540, 1380, 1220, 1060,  900,  740,  580,  420,  260,  100};

Int_t bemcBuilder::pmtbxMinSoftId[] = {2261, 2181, 2101, 2021, 1941, 1861, 1781, 1701, 1621, 1541, 1461, 1381, 1301, 1221, 1141, 1061,  981,  901,  821,  741,  661,  581,  501,  421,  341,  261,  181,  101,   21, 2341/*and from 1*/,
				       4661, 4741, 2421, 2501, 2581, 2661, 2741, 2821, 2901, 2981, 3061, 3141, 3221, 3301, 3381, 3461, 3541, 3621, 3701, 3781, 3861, 3941, 4021, 4101, 4181, 4261, 4341, 4421, 4501, 4581
				       /*and from 2401*/
};
Int_t bemcBuilder::pmtbxMaxSoftId[] = {2340, 2260, 2180, 2100, 2020, 1940, 1860, 1780, 1700, 1620, 1540, 1460, 1380, 1300, 1220, 1140, 1060,  980,  900,  820,  740,  660,  580,  500,  420,  340,  260,  180,  100, 2400/*to 20*/,
				       4740, 4800, 2500, 2580, 2660, 2740, 2820, 2900, 2980, 3060, 3140, 3220, 3300, 3380, 3460, 3540, 3620, 3700, 3780, 3860, 3940, 4020, 4100, 4180, 4260, 4340, 4420, 4500, 4580, 4660
				       /*to 2420*/
};
 
#define FIND_HISTO(TYPE, NAME, FILE, TITLE) \
TYPE *NAME = (TYPE*)GetHisto((FILE), (TITLE)); \
if (!NAME) { \
  LOG(ERR, "Histogram " NAME " not found: " TITLE);	\
}

bemcBuilder::bemcBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"bemc";

  numPmtBoxes = sizeof(pmtbxMinSoftId)/sizeof(Int_t);
  numBoxes = sizeof(crateMinSoftId)/sizeof(Int_t);

  BEMCDecoderPresenter = new StEmcDecoder();
}

bemcBuilder::~bemcBuilder() {
}

// panel=0 "Status"
// panel=1 "Towers"
// panel=2 "SMD/PSD"
// panel=3 "Trigger"
// panel=4 "Jet"
// panel=5 "BTOW ADC"
// panel=6 "JetPatch HighTower Spectra"
// panel=7 "JetPatch PatchSum Spectra"
// panel=8 "DSM Level-0 Input"
// panel=9 "DSM Level-1 Input"
// panel=10 "DSM level-2 Input"
// panel=11 "BSMD FEE Sum"
// panel=12 "Trigger corruption"
// panel=13 "BPRS FEE Sum"
// panel=14 "BPRS ADC"



// min = min soft id array
// max = max soft id array
void bemcBuilder::addCrateIDs(JevpPlot *plot, Int_t numBoxes, Int_t *minc, Int_t *maxc, const char *labelFormat)
{
  //Int_t linesColor = kRed;
  plot->gridx = 0;
  plot->gridy = 0;
  TH1 *hist = plot->getHisto(0)->histo;

  for (Int_t crate = 0;crate < numBoxes;crate++) {
    if (!((maxc[crate] < hist->GetXaxis()->GetBinLowEdge(1))
	  || (minc[crate] > hist->GetXaxis()->GetBinUpEdge(hist->GetXaxis()->GetNbins())))
	) {
      Float_t left = max(minc[crate], hist->GetXaxis()->GetBinCenter(1)) - 0.5;
      Float_t right = 0.5 + min(maxc[crate], hist->GetXaxis()->GetBinCenter(hist->GetXaxis()->GetNbins()));
      TLine *lCrates = new TLine(left, hist->GetYaxis()->GetBinLowEdge(1), left, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
      if (lCrates) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
	lCrates->SetLineColor(linesColor); 
	lCrates->SetLineWidth(1);
	plot->addElement(lCrates);
      }
      TLine *rCrates = new TLine(right, hist->GetYaxis()->GetBinLowEdge(1), right, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
      if (rCrates) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(rCrates);
	rCrates->SetLineColor(linesColor); 
	rCrates->SetLineWidth(1);
	plot->addElement(rCrates);
      }
      TString crateLabel;
      crateLabel = Form(labelFormat, crate + 1);
      TLatex *textCrate = new TLatex(left + 10, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()) * 0.8, crateLabel.Data());
      if (textCrate) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
	textCrate->SetTextColor(linesColor);
	//textCrate->SetTextSize(0.14);
	textCrate->SetTextSize(.06);
	plot->addElement(textCrate);
      }
    }
  }
}

void bemcBuilder::addDsmL2TowerIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  plot->gridy = 0;
 
  plot->getHisto(0)->histo->GetXaxis()->SetNdivisions(12, false);
  plot->getHisto(0)->histo->GetYaxis()->SetNdivisions(0, false);

  TLatex *textHT0 = new TLatex(-1.4, 0.5, "HT < th0");
  if (textHT0) {
    //  if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
    textHT0->SetTextColor(linesColor);
    textHT0->SetTextSize(0.05);
    plot->addElement(textHT0);
  }
  TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < HT < th1");
  if (textHT1) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
    textHT1->SetTextColor(linesColor);
    textHT1->SetTextSize(0.05);
    plot->addElement(textHT1);
  }
  TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < HT < th2");
  if (textHT2) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
    textHT2->SetTextColor(linesColor);
    textHT2->SetTextSize(0.05);
    
    plot->addElement(textHT2);
  }
  TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < HT");
  if (textHT3) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
    textHT3->SetTextColor(linesColor);
    textHT3->SetTextSize(0.05);
    plot->addElement(textHT3);
  }
  TLatex *textDsmsL1 = new TLatex(-1.4, 4.5, "#splitline{DSM Level-1}{Boards}");
  if (textDsmsL1) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL1);
    textDsmsL1->SetTextColor(linesColor);
    textDsmsL1->SetTextSize(0.05);
    plot->addElement(textDsmsL1);
  }
  for (Int_t ch = 0;ch < 12;ch += 2) {
    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 5);
    if (lDsmL1Begin) {
      //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
      lDsmL1Begin->SetLineColor(linesColor);
      lDsmL1Begin->SetLineWidth(1);
      plot->addElement(lDsmL1Begin);
    }
    TString label;
    if (ch < 6) {
      label = Form("BW1%.2u", ((ch / 2) % 3) + 1);
    } else {
      label = Form("BE1%.2u", ((ch / 2) % 3) + 1);
    }
    TLatex *text = new TLatex(ch + 0.1-0.5, 4.5, label.Data());
    if (text) {
      //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
      text->SetTextColor(linesColor);
      text->SetTextSize(0.05);
      plot->addElement(text);
    }
  }
}

void bemcBuilder::addDsmL2PatchIndex(JevpPlot *plot)
{
  plot->getHisto(0)->histo->GetXaxis()->SetNdivisions(12, false);
  plot->getHisto(0)->histo->GetYaxis()->SetNdivisions(0, false);

  TLatex *textHT0 = new TLatex(-1.4, 0.5, "Sum < th0");
  if (textHT0) {
    //  if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
    textHT0->SetTextColor(linesColor);
    textHT0->SetTextSize(0.05);
    plot->addElement(textHT0);
  }
  TLatex *textHT1 = new TLatex(-1.4, 1.5, "th0 < Sum < th1");
  if (textHT1) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
    textHT1->SetTextColor(linesColor);
    textHT1->SetTextSize(0.05);
    plot->addElement(textHT1);
  }
  TLatex *textHT2 = new TLatex(-1.4, 2.5, "th1 < Sum < th2");
  if (textHT2) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
    textHT2->SetTextColor(linesColor);
    textHT2->SetTextSize(0.05);
    plot->addElement(textHT2);
  }
  TLatex *textHT3 = new TLatex(-1.4, 3.5, "th2 < Sum");
  if (textHT3) {
    // if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
    textHT3->SetTextColor(linesColor);
    textHT3->SetTextSize(0.05);
    plot->addElement(textHT3);
  }
  for (Int_t ch = 0;ch < 12;ch += 2) {
    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 4);
    if (lDsmL1Begin) {
      //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
      lDsmL1Begin->SetLineColor(linesColor);
      lDsmL1Begin->SetLineWidth(1);
      plot->addElement(lDsmL1Begin);
      lDsmL1Begin->Draw();
    }
  }
}

void bemcBuilder::addDsmL2PatchSumIndex(JevpPlot *plot)
{
  for (Int_t ch = 0;ch < 6;ch++) {
    TLine *lDsmL1Begin = new TLine(ch-0.5, 0, ch-0.5, 256);
    if (lDsmL1Begin) {
      //     if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL1Begin);
      lDsmL1Begin->SetLineColor(linesColor);
      lDsmL1Begin->SetLineWidth(1);
      plot->addElement(lDsmL1Begin);
    }
  }
}

void bemcBuilder::addDsmL1PatchIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  plot->gridy = 0;

  Int_t dsmL0 = 0;
  Int_t ch = 0;
  for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
    for (Int_t idsmL1ch = 0;idsmL1ch < 6;idsmL1ch++) {		
      TLine *lDsmL0Begin = new TLine(ch-0.5, 0, ch-0.5, 1024);
      if (lDsmL0Begin) {
	//	if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL0Begin);
	lDsmL0Begin->SetLineColor(linesColor);
	lDsmL0Begin->SetLineWidth(1);
	plot->addElement(lDsmL0Begin);
      }
      Int_t crate = 0, crateSeq = 0;
      if (BEMCDecoderPresenter) {
	Int_t triggerPatch = dsmL0 * 10;
	BEMCDecoderPresenter->GetCrateAndSequenceFromTriggerPatch(triggerPatch, crate, crateSeq);
      }
      TString label;
      label = Form("%.2X", crate);
      TLatex *text = new TLatex(ch + 0.1-0.5, 220, label.Data());
      if (text) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
	text->SetTextColor(linesColor);
	text->SetTextSize(0.0222);
	plot->addElement(text);
      }
      ch++;	
      if (idsmL1ch != 2) dsmL0++;
    }
  }
}

void bemcBuilder::addDsmL1TowerIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  plot->gridy = 0;

  TLatex *textHT0 = new TLatex(-3.5, 0.5, "HT < th0");
  if (textHT0) {
    //    if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT0);
    textHT0->SetTextColor(linesColor);
    textHT0->SetTextSize(0.0222);
    plot->addElement(textHT0);
  }
  TLatex *textHT1 = new TLatex(-3.5, 1.5, "th0 < HT < th1");
  if (textHT1) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT1);
    textHT1->SetTextColor(linesColor);
    textHT1->SetTextSize(0.0222);
    plot->addElement(textHT1);
  }
  TLatex *textHT2 = new TLatex(-3.5, 2.5, "th1 < HT < th2");
  if (textHT2) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT2);
    textHT2->SetTextColor(linesColor);
    textHT2->SetTextSize(0.0222);
    plot->addElement(textHT2);
  }
  TLatex *textHT3 = new TLatex(-3.5, 3.5, "th2 < HT");
  if (textHT3) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textHT3);
    textHT3->SetTextColor(linesColor);
    textHT3->SetTextSize(0.0222);
    plot->addElement(textHT3);
  }
  TLatex *textDsmsL0 = new TLatex(-3.5, 4.0, "#splitline{#splitline{DSM Level-0}{Boards}}{BTOW Crates}");
  if (textDsmsL0) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL0);
    textDsmsL0->SetTextColor(linesColor);
    textDsmsL0->SetTextSize(0.0222);
    plot->addElement(textDsmsL0);
  }


  Int_t dsmL0 = 0;
  Int_t ch = 0;
  for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
    for (Int_t idsmL1ch = 0;idsmL1ch < 6;idsmL1ch++) {		
      TLine *lDsmL0Begin = new TLine(ch-0.5, 0, ch-0.5, 4.4);
      if (lDsmL0Begin) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lDsmL0Begin);
	lDsmL0Begin->SetLineColor(linesColor);
	lDsmL0Begin->SetLineWidth(1);
	plot->addElement(lDsmL0Begin);
      }
      TString label;
      Int_t crate = 0, crateSeq = 0;
      if (BEMCDecoderPresenter) {
	Int_t triggerPatch = dsmL0 * 10;
	BEMCDecoderPresenter->GetCrateAndSequenceFromTriggerPatch(triggerPatch, crate, crateSeq);
      }
      if (idsmL1 < 3) {
	label = Form("#splitline{#splitline{BW}{0%.2u}}{%.2X}", (dsmL0 % 15) + 1, crate);
      } else {
	label = Form("#splitline{#splitline{BE}{0%.2u}}{%.2X}", (dsmL0 % 15) + 1, crate);
      }
      TLatex *text = new TLatex(ch + 0.1-0.5, 4.0, label.Data());
      if (text) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
	text->SetTextColor(linesColor);
	text->SetTextSize(0.0222);
	plot->addElement(text);
      }
      ch++;	
      if (idsmL1ch != 2) dsmL0++;
    }
  }
  for (Int_t ijp = 0;ijp < 12;ijp++) {
    Int_t maxCh = (ijp * 3) + 3;
    Int_t minCh = (ijp * 3) + 0;
    Float_t height = (ijp%2) ? 4.7 : 4.7;
    TArrow *arrow = new TArrow(minCh-0.5, height, maxCh-0.5, height, 0.02, "<>");
    if (arrow) {
      // if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
      arrow->SetLineColor(linesColor);
      arrow->SetFillColor(linesColor);
      plot->addElement(arrow);
    }
    TString jpLabel;
    jpLabel = Form("JetPatch %u", ijp);
    TLatex *text = new TLatex(minCh + 0.1-0.5, height + 0.1, jpLabel.Data());
    if (text) {
      // if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
      text->SetTextColor(linesColor);
      text->SetTextSize(0.0222);
      plot->addElement(text);
    }
  }
  TLatex *textDsmsL1 = new TLatex(-3.5, -0.4, "#splitline{DSM Level-1}{Boards}");
  if (textDsmsL1) {
    // if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textDsmsL1);
    textDsmsL1->SetTextColor(linesColor);
    textDsmsL1->SetTextSize(0.0222);
    plot->addElement(textDsmsL1);
  }
  for (Int_t idsmL1 = 0;idsmL1 < 6;idsmL1++) {
    TArrow *arrow = new TArrow((idsmL1 * 6)-0.5, -0.4, (idsmL1 * 6) + 6-0.5, -0.4, 0.02, "<>");
    if (arrow) {
      //  if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
      arrow->SetLineColor(linesColor);
      arrow->SetFillColor(linesColor);
      plot->addElement(arrow);
    }
    TString label;
    if (idsmL1 < 3) {
      label = Form("BW1%.2u", (idsmL1 % 3) + 1);
    } else {
      label = Form("BE1%.2u", (idsmL1 % 3) + 1);	    
    }
    TLatex *text = new TLatex((idsmL1 * 6) + 1-0.5, -0.35, label.Data());
    if (text) {
      //  if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
      text->SetTextColor(linesColor);
      text->SetTextSize(0.0222);
      plot->addElement(text);
    }
  } 
}

void bemcBuilder::addTowerIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  plot->gridy = 0;

  TLine *lCrates = new TLine(0, 50, 300, 50);
  if (lCrates) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrates);
    lCrates->SetLineColor(linesColor); 
    lCrates->SetLineWidth(1);
    plot->addElement(lCrates);
  }
  TLatex *textCrates = new TLatex(1, 50, "BTOW Crates:");
  if (textCrates) {
    //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrates);
    textCrates->SetTextColor(linesColor);
    textCrates->SetTextSize(0.0333);
    plot->addElement(textCrates);
  }
  for (Int_t icrate = 1;icrate <= 30;icrate++) {
    Int_t triggerPatchBegin, triggerPatchEnd;
    if (BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 0, triggerPatchBegin) && BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 159, triggerPatchEnd)) {
      TLine *lCrateBegin = new TLine(triggerPatchBegin-0.5, 0, triggerPatchBegin-0.5, 50);
      if (lCrateBegin) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrateBegin);
	lCrateBegin->SetLineColor(linesColor);
	lCrateBegin->SetLineWidth(1);
	plot->addElement(lCrateBegin);
      }
      TString crateLabel;
      crateLabel = Form("%.2X", icrate);
      TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 45, crateLabel.Data());
      if (textCrate) {
	//if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
	textCrate->SetTextColor(linesColor);
	textCrate->SetTextSize(0.0333);
	plot->addElement(textCrate);
      }
    }
  }
  for (Int_t ijp = 0;ijp < 12;ijp++) {
    Int_t maxTriggerPatch = 0;
    Int_t minTriggerPatch = 300;
    for (Int_t j = 0;j < 25;j++) {
      Int_t triggerPatch;
      if (BEMCDecoderPresenter->GetTriggerPatchFromJetPatch(ijp, j, triggerPatch)) {
	if (maxTriggerPatch < triggerPatch) maxTriggerPatch = triggerPatch;
	if (minTriggerPatch > triggerPatch) minTriggerPatch = triggerPatch;
      }
    }
    maxTriggerPatch++;
    Int_t height = (ijp%2) ? 55 : 60;
    TArrow *arrow = new TArrow(minTriggerPatch-0.5, height, maxTriggerPatch-0.5, height, 0.02, "<>");
    if (arrow) {
      //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(arrow);
      arrow->SetLineColor(linesColor);
      arrow->SetFillColor(linesColor);
      plot->addElement(arrow);
    }
    TString jpLabel;
    jpLabel = Form("JetPatch %u", ijp);
    TLatex *text = new TLatex(minTriggerPatch + 1, height + 1, jpLabel.Data());
    if (text) {
      //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
      text->SetTextColor(linesColor);
      text->SetTextSize(0.0222);
      plot->addElement(text);
    }
  }
}

void bemcBuilder::addPatchSumIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  plot->gridy = 0;
  for (Int_t icrate = 1;icrate <= 30;icrate++) {
    Int_t triggerPatchBegin, triggerPatchEnd;
    if (BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 0, triggerPatchBegin) && BEMCDecoderPresenter->GetTriggerPatchFromCrate(icrate, 159, triggerPatchEnd)) {
      TLine *lCrateBegin = new TLine(triggerPatchBegin-0.5, 0, triggerPatchBegin-0.5, 64);
      if (lCrateBegin) {
	// if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lCrateBegin);
	lCrateBegin->SetLineColor(linesColor);
	lCrateBegin->SetLineWidth(1);
	plot->addElement(lCrateBegin);
      }
      TString crateLabel;
      crateLabel = Form("%.2X", icrate);
      TLatex *textCrate = new TLatex(triggerPatchBegin + 1, 60, crateLabel.Data());
      if (textCrate) {
	// if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(textCrate);
	textCrate->SetTextColor(linesColor);
	textCrate->SetTextSize(0.0333);
	plot->addElement(textCrate);
      }
    }
  }
}

void bemcBuilder::addSmdFeeSumIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  TH1 *hist = plot->getHisto(0)->histo;

  Int_t moduleRdo[120];
  if(BEMCDecoderPresenter) {
    Int_t RDO, index;
    for (Int_t module = 1;module <= 120;module++) {
      moduleRdo[module - 1] = -1;
      if (BEMCDecoderPresenter->GetSmdRDO(3, module, 1, 1, RDO, index)) {
	moduleRdo[module - 1] = RDO;
      }
      //cout << "module " << module << ": RDO " << moduleRdo[module - 1] << endl;
    }
  }
  Int_t curmod = 1;
  Int_t currdo = -1;
  Int_t beginrdo = -1;
  Int_t endrdo = -1;
  while (curmod <= 121) {
    //cout << "curmod = " << curmod << ", currdo = " << currdo << ", beginrdo = " << beginrdo << ", endrdo = " << endrdo << endl;
    if (((curmod <= 120) && (moduleRdo[curmod - 1] != currdo)) || (curmod == 121)) {
      if ((currdo == -1) && (curmod <= 120)) {
	beginrdo = curmod;
	currdo = moduleRdo[curmod - 1];
	curmod++;
      } else {
	endrdo = curmod - 1;
	TLine *lRdoBegin = new TLine(beginrdo - 0.5, hist->GetYaxis()->GetBinLowEdge(1), beginrdo - 0.5, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
	if (lRdoBegin) {
	  //  if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoBegin);
	  lRdoBegin->SetLineColor(linesColor);
	  lRdoBegin->SetLineWidth(1);
	  plot->addElement(lRdoBegin);
	}
	TLine *lRdoEnd = new TLine(endrdo + 0.5, hist->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
	if (lRdoEnd) {
	  //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoEnd);
	  lRdoEnd->SetLineColor(linesColor);
	  lRdoEnd->SetLineWidth(1);
	  plot->addElement(lRdoEnd);
	}
	TString label;
	label = Form("RDO %i", currdo);
	TLatex *text = new TLatex(beginrdo + 1 -0.5, 0.8 * hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()), label.Data());
	if (text) {
	  //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
	  text->SetTextColor(linesColor);
	  text->SetTextSize(0.03);
	  plot->addElement(text);
	}
	currdo = -1;
	if (curmod > 120) curmod++;
      }
    } else {
      curmod++;
    }
  }
}

void bemcBuilder::addPsdFeeSumIndex(JevpPlot *plot)
{
  plot->gridx = 0;
  TH1 *hist = plot->getHisto(0)->histo;


  hist->SetStats(0);
  hist->Draw("COLZ");
  Int_t pmtRdo[60];
  for (Int_t box = 0;box < 60;box++) pmtRdo[box] = -1;
  if(BEMCDecoderPresenter) {
    for (Int_t rdo = 0;rdo < 4;rdo++) {
      for (Int_t index = 0;index < 4800;index++) {
	Int_t id, box, wire, Avalue;
	if (BEMCDecoderPresenter->GetPsdId(rdo, index, id, box, wire, Avalue)) {
	  if ((box >= 1) && (box <= 60)) {
	    pmtRdo[box - 1] = rdo + 8;
	  }
	}
      }
    }
  }
  Int_t curmod = 1;
  Int_t currdo = -1;
  Int_t beginrdo = -1;
  Int_t endrdo = -1;
  while (curmod <= 61) {
    //cout << "curmod = " << curmod << ", currdo = " << currdo << ", beginrdo = " << beginrdo << ", endrdo = " << endrdo << endl;
    if (((curmod <= 60) && (pmtRdo[curmod - 1] != currdo)) || (curmod == 61)) {
      if ((currdo == -1) && (curmod <= 60)) {
	beginrdo = curmod;
	currdo = pmtRdo[curmod - 1];
	curmod++;
      } else {
	endrdo = curmod - 1;
	TLine *lRdoBegin = new TLine(beginrdo - 0.5, hist->GetYaxis()->GetBinLowEdge(1), beginrdo - 0.5, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
	if (lRdoBegin) {
	  //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoBegin);
	  lRdoBegin->SetLineColor(linesColor);
	  lRdoBegin->SetLineWidth(1);
	  plot->addElement(lRdoBegin);
	}
	TLine *lRdoEnd = new TLine(endrdo + 0.5, hist->GetYaxis()->GetBinLowEdge(1), endrdo + 0.5, hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()));
	if (lRdoEnd) {
	  //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(lRdoEnd);
	  lRdoEnd->SetLineColor(linesColor);
	  lRdoEnd->SetLineWidth(1);
	  plot->addElement(lRdoEnd);
	}
	TString label;
	label = Form("RDO %i", currdo);
	TLatex *text = new TLatex(beginrdo + 0.5 -0.5, 0.8 * hist->GetYaxis()->GetBinUpEdge(hist->GetYaxis()->GetNbins()), label.Data());
	if (text) {
	  //if (BEMCPlotsCleanUp) BEMCPlotsCleanUp->Add(text);
	  text->SetTextColor(linesColor);
	  text->SetTextSize(0.03);
	  plot->addElement(text);
	}
	currdo = -1;
	if (curmod > 60) curmod++;
      }
    } else {
      curmod++;
    }
  }
}


void bemcBuilder::initialize(int argc, char *argv[]) {
  sprintf(statusfile, "%s/bemc/bemcStatus.txt", confdatadir);
  printf("Statusfile: %s\n",statusfile);
  BEMCPlots::initHisto(0, statusfile);

  TH1 *hist;

  // Add root histograms to Plots
  int np = 200;
  int n=0;
  JevpPlot *plots[np];

  gROOT->GetObject(Hist_TDC_statusName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_TDC_statusName);
  plots[n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_SMD_statusName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_SMD_statusName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_PSD_statusName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_PSD_statusName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_BTOW_CorruptionName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_BTOW_CorruptionName);
  plots[++n] = new JevpPlot(hist); 
  plots[n]->setDrawOpts((char *)"H COLZ");
  
  gROOT->GetObject(Hist_btow_spectra_1Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_btow_spectra_1Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_btow_spectra_2Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_btow_spectra_2Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_btow_spectra_3Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_btow_spectra_3Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_smd_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_smd_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_smd_spectraNonZSName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_smd_spectraNonZSName);
  hist->SetLineColor(kRed);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_smd_capacitorName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_smd_capacitorName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");
  
  gROOT->GetObject(Hist_smd_sumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_smd_sumName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_psd_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_psd_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_psd_spectraNonZSName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_psd_spectraNonZSName);
  hist->SetLineColor(kRed);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");

  gROOT->GetObject(Hist_psd_capacitorName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_psd_capacitorName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");
  
  gROOT->GetObject(Hist_psd_sumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_psd_sumName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  
  gROOT->GetObject(Hist_HTMAX_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_HTMAX_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_PAMAX_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_PAMAX_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_HTMAX_distName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_HTMAX_distName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_PAMAX_distName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_PAMAX_distName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_JET_pedName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_JET_pedName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_JET_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_JET_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_JETMAX_spectraName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_JETMAX_spectraName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(Hist_JETMAX_distName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_JETMAX_distName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  

  gROOT->GetObject(HistRawAdc1Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdc1Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numBoxes, crateMinSoftId, crateMaxSoftId, "0x%X");
  
  gROOT->GetObject(HistRawAdc2Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdc2Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numBoxes, crateMinSoftId, crateMaxSoftId, "0x%X");
  
  gROOT->GetObject(HistRawAdc3Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdc3Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numBoxes, crateMinSoftId, crateMaxSoftId, "0x%X");
  
  gROOT->GetObject(HistRawAdc4Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdc4Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numBoxes, crateMinSoftId, crateMaxSoftId, "0x%X");

  gROOT->GetObject(HistRawAdcPsd1Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdcPsd1Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numPmtBoxes, pmtbxMinSoftId, pmtbxMaxSoftId, "%d");

  gROOT->GetObject(HistRawAdcPsd2Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdcPsd2Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numPmtBoxes, pmtbxMinSoftId, pmtbxMaxSoftId, "%d");
  
  gROOT->GetObject(HistRawAdcPsd3Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdcPsd3Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numPmtBoxes, pmtbxMinSoftId, pmtbxMaxSoftId, "%d");
  
  gROOT->GetObject(HistRawAdcPsd4Name, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistRawAdcPsd4Name);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"H COLZ");  
  addCrateIDs(plots[n], numPmtBoxes, pmtbxMinSoftId, pmtbxMaxSoftId, "%d");

  //  Additional drawings?
  //
  //
  //
 
  for(Int_t jet=0;jet<12;jet++) {
    char nm[256];
    sprintf(nm,"%s_%u",HistHighTowerSpectrumName, jet);
    gROOT->GetObject(nm, hist);
    if(!hist) LOG("ERR", "couldn't find %s",nm);
    plots[++n] = new JevpPlot(hist);
    plots[n]->logy = 1;
  }

  for(Int_t jet=0;jet<12;jet++) {
    char nm[256];
    sprintf(nm,"%s_%u",HistPatchSumSpectrumName, jet);
    gROOT->GetObject(nm, hist);
    if(!hist) LOG("ERR", "couldn't find %s",nm);
    plots[++n] = new JevpPlot(hist);
    plots[n]->logy = 1;
  }

  gROOT->GetObject(HistDsmL0InputHighTowerName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL0InputHighTowerName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"COLZ");   
  addTowerIndex(plots[n]);

  gROOT->GetObject(HistDsmL0InputPatchSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL0InputPatchSumName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"COLZ");  
  addPatchSumIndex(plots[n]);

  // More stuff....
  //
  //
  //
  gROOT->GetObject(HistDsmL1InputHighTowerBitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL1InputHighTowerBitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"COLZ");   
  addDsmL1TowerIndex(plots[n]);

  gROOT->GetObject(HistDsmL1InputPatchSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL1InputPatchSumName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"COLZ");  
  addDsmL1PatchIndex(plots[n]);

  gROOT->GetObject(HistDsmL2InputHighTowerBitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL2InputHighTowerBitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("COLZ");
  addDsmL2TowerIndex(plots[n]);

  gROOT->GetObject(HistDsmL2InputPatchSumBitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL2InputPatchSumBitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts((char *)"COLZ");  
  addDsmL2PatchIndex(plots[n]);

  gROOT->GetObject(HistDsmL2InputPatchSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL2InputPatchSumName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("COLZ");
  addDsmL2PatchSumIndex(plots[n]);

  gROOT->GetObject(HistDsmL3InputHighTowerBitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL3InputHighTowerBitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("COLZ");

  gROOT->GetObject(HistDsmL3InputPatchSumBitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL3InputPatchSumBitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("COLZ");

  gROOT->GetObject(HistDsmL3InputBackToBackBitName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL3InputBackToBackBitName);
  plots[++n] = new JevpPlot(hist);

  gROOT->GetObject(HistDsmL3InputJPsiTopoBitName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL3InputJPsiTopoBitName);
  plots[++n] = new JevpPlot(hist);

  gROOT->GetObject(HistDsmL3InputJetPatchTopoBitName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDsmL3InputJetPatchTopoBitName);
  plots[++n] = new JevpPlot(hist);

  gROOT->GetObject(HistSmdFeeSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistSmdFeeSumName);
  plots[++n] = new JevpPlot(hist);
  addSmdFeeSumIndex(plots[n]);

  gROOT->GetObject(HistSmdFeeSumNonZSName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistSmdFeeSumNonZSName);
  plots[++n] = new JevpPlot(hist);
  addSmdFeeSumIndex(plots[n]);

  gROOT->GetObject(HistPsdFeeSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistPsdFeeSumName);
  plots[++n] = new JevpPlot(hist);
  addPsdFeeSumIndex(plots[n]);

  gROOT->GetObject(HistPsdFeeSumNonZSName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistPsdFeeSumNonZSName);
  plots[++n] = new JevpPlot(hist);
  addPsdFeeSumIndex(plots[n]);

  gROOT->GetObject(HistTriggerCorruptionHighTowerName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistTriggerCorruptionHighTowerName);
  plots[++n] = new JevpPlot(hist);

  gROOT->GetObject(HistTriggerCorruptionPatchSumName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistTriggerCorruptionPatchSumName);
  plots[++n] = new JevpPlot(hist);
  
  gROOT->GetObject(HistDSM0HTCorrName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDSM0HTCorrName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("H COLZ");

  gROOT->GetObject(HistDSM0TPCorrName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",HistDSM0TPCorrName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("H COLZ");

  gROOT->GetObject(Hist_ADCEtaPhi_TowHitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_ADCEtaPhi_TowHitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("H COLZ");

  gROOT->GetObject(Hist_ADCEtaPhi_Pre1HitsName, hist);
  if(!hist) LOG("ERR", "couldn't find %s",Hist_ADCEtaPhi_Pre1HitsName);
  plots[++n] = new JevpPlot(hist);
  plots[n]->setDrawOpts("H COLZ");  

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d: %s",i,plots[i]->GetPlotName());
    plots[i]->optstat = 10;
    addPlot(plots[i]);
  }
}
  
void bemcBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "bemcBuilder starting run #%d",rdr->run);
 
  BEMCPlots::resetHisto(statusfile);
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void bemcBuilder::event(daqReader *rdr)
{
  // Find trigger data...
  //int run = rdr->run;
  
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  BEMCPlots::fillHisto( (char *)rdr,
			trgd ? trgd->getDsm0_BEMCE() : 0,
			trgd ? trgd->getDsm0_BEMCW() : 0,
			trgd ? trgd->getDsm1_BEMC() : 0,
			trgd ? trgd->getDsm2_EMC() : 0,
			trgd ? trgd->getDsm3() : 0);

  if(trgd) delete trgd;
}

void bemcBuilder::stoprun(daqReader *rdr) {
  
}

void bemcBuilder::main(int argc, char *argv[])
{
  bemcBuilder me;
  
  me.Main(argc, argv);
}

