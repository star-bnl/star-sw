/***************************************************************************
 *
 * $Id: StSvtHybridGraph.cc,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Histogram BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridGraph.cc,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/

#include "StSvtHybridGraph.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"

#include "TH1.h"
#include "TGraph.h"
#include "TArrayF.h"
#include "TStyle.h"

ClassImp(StSvtHybridGraph)

StSvtHybridGraph::StSvtHybridGraph() : StSvtHybridObject()
{}

StSvtHybridGraph::StSvtHybridGraph(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  graph = new TGraph();
}

StSvtHybridGraph::~StSvtHybridGraph()
{
  delete graph;
}

TH1F* StSvtHybridGraph::getHist()
{
  return graph->GetHistogram();
}

void StSvtHybridGraph::Fill(int nEvents, StSvtHybridCollection* collEvent, StSvtHybridCollection* collData)
{
  TArrayF* array;
  TArrayF* n_array;

  int index_hyb = collEvent->getHybridIndex(mBarrel, mLadder, mWafer, mHybrid);

  array = (TArrayF*)collData->at(index_hyb);
  n_array = (TArrayF*)collEvent->at(index_hyb);

  if (nEvents && array && n_array) {

    if (graph) {
      if (graph->GetN() != nEvents) { 
	if (graph->GetHistogram()) {
	  mMaximum = graph->GetHistogram()->GetMaximum();
	  mMinimum = graph->GetHistogram()->GetMinimum();
	}

	delete graph;
	graph = new TGraph(nEvents,n_array->GetArray(),array->GetArray());
      }
    }
    else
      graph = new TGraph(nEvents,n_array->GetArray(),array->GetArray());
  }
}

void StSvtHybridGraph::Fill(int nEvents, TArrayF* n_array, TArrayF* array)
{
  if (nEvents && array && n_array) {

    if (graph) {
      if (graph->GetN() != nEvents) { 
	if (graph->GetHistogram()) {
	  mMaximum = graph->GetHistogram()->GetMaximum();
	  mMinimum = graph->GetHistogram()->GetMinimum();
	}

	delete graph;
	graph = new TGraph(nEvents,n_array->GetArray(),array->GetArray());
      }
    }
    else
      graph = new TGraph(nEvents,n_array->GetArray(),array->GetArray());
  }
}

void StSvtHybridGraph::Draw(char* option)
{
  if (graph->GetN()) { 
    graph->SetMarkerStyle(20);
    graph->SetMinimum(mMinimum);
    graph->SetMaximum(mMaximum);
    graph->SetTitle(mTitle);
    graph->Draw("AP");
    graph->GetHistogram()->SetXTitle(mXTitle);
    graph->GetHistogram()->SetYTitle(mYTitle);
    if ( !strncmp(option, "LADDER", strlen("LADDER")) ) {
      gStyle->SetOptTitle(0);
      gStyle->SetStatH(.2);
      gStyle->SetStatW(.3);
      graph->GetHistogram()->GetXaxis()->SetTitleSize(.07);
      graph->GetHistogram()->GetYaxis()->SetTitleSize(.07);
      graph->GetHistogram()->SetLabelSize(.07,"X");
      graph->GetHistogram()->SetLabelSize(.07,"Y");
      graph->GetHistogram()->SetNdivisions(406);
    }
    else {
      gStyle->SetTitleW(.75);
      gStyle->SetOptTitle(1);
      gStyle->SetStatH();
      gStyle->SetStatW();
      graph->GetHistogram()->GetXaxis()->SetTitleSize(.04);
      graph->GetHistogram()->GetYaxis()->SetTitleSize(.04);
      graph->GetHistogram()->SetLabelSize(.04,"X");
      graph->GetHistogram()->SetLabelSize(.04,"Y");
      graph->GetHistogram()->SetNdivisions(510);
    }
  }
}

void StSvtHybridGraph::setTitle(Text_t* title)
{
  char suffix[100];
  sprintf(suffix," - Barrel #%d, Ladder #%d, Wafer #%d, Hybrid #%d",
	  mBarrel,mLadder,mWafer,mHybrid);

  mTitle = TString(title) + TString(suffix);
}

void StSvtHybridGraph::setThisTitle(Text_t* title)
{
  mTitle = TString(title);
}
