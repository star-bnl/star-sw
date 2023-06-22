/******************************************************************************
 * $Id: StGlauberPlotMaker.cxx,v 1.2 2012/04/25 05:02:16 hmasui Exp $
 * $Log: StGlauberPlotMaker.cxx,v $
 * Revision 1.2  2012/04/25 05:02:16  hmasui
 * 5% increment for centrality bins. Added 3rd harmonic eccentricity
 *
 ******************************************************************************/

#include <assert.h>
#include <fstream>

#include "TColor.h"
#include "TCanvas.h"
#include "TError.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TH1.h"
#include "TLegend.h"
#include "TLine.h"
#include "TMath.h"
#include "TStyle.h"

#include "StMessMgr.h"

#include "StGlauberConstUtilities.h"
#include "StGlauberPlotMaker.h"

ClassImp(StGlauberPlotMaker)

	using std::ifstream ;
	using std::ofstream ;
	using std::endl ;
	using std::vector ;

	UInt_t StGlauberPlotMaker::mCanvasId = 0 ;
	UInt_t StGlauberPlotMaker::mGraphId  = 0 ;

	//____________________________________________________________________________________________________
	// Default constructor
	StGlauberPlotMaker::StGlauberPlotMaker(const TString name)
: mName(name)
{
	mGraph.clear() ;
	mGraphDraw.clear() ;
	mSystematicError = 0 ;
}

//____________________________________________________________________________________________________
// Default destructor
StGlauberPlotMaker::~StGlauberPlotMaker()
{
	mGraph.clear() ;
	mGraphDraw.clear() ;
	if(mSystematicError) delete mSystematicError ;
}

//____________________________________________________________________________________________________
Int_t StGlauberPlotMaker::Read(const TString filename, const TString type)
{
	ifstream fin(filename.Data());
	if(!fin){
		Error("StGlauberPlotMaker::Read", "can't open %s", filename.Data());
		return 0 ;
	}
	
	LOG_INFO << Form("StGlauberPlotMaker::Read  OPEN %s (%s)", filename.Data(), type.Data()) << endm;

	TGraphErrors* gall  = new TGraphErrors() ;
	TGraphErrors* gdraw = new TGraphErrors() ;
	const TString name("g" + mName) ;
	gall ->SetName(Form("%s_all_%d", name.Data(), mGraphId));
	gdraw->SetName(Form("%s_draw_%d", name.Data(), mGraphId));
	gall ->SetTitle(type);
	gdraw->SetTitle(type);
	mGraphId++; // increment graph id

	LOG_INFO << "StGlauberPlotMaker::Read  Init graphs: "
		<< gall->GetName() << " and " << gdraw->GetName()
		<< endm;

	const UInt_t ncent = mNCentrality ; // 16 centrality bins for 0-80% (5% increment in 0-80%)
	UInt_t   centralityId ;
	Double_t centMin, centMax ;
	Double_t val, error ;
	
	for(UInt_t ic=0; ic<StGlauberConstUtilities::GetCentralityBin(); ic++)
	{
		fin >> centralityId >> centMin >> centMax >> val >> error ;

		const Double_t cent = (centMin + centMax)/2.0 ;
		if( centralityId < ncent ){
			gdraw->SetPoint(centralityId, cent, val);
			gdraw->SetPointError(centralityId, 0.0, error);
		}

		LOG_INFO << "StGlauberPlotMaker::Read  " << mName << ": centrality (min,max) = ("
			<< centMin << "," << centMax << "), val = "
			<< val << " +/- " << error
			<< endm;

		gall->SetPoint(centralityId, cent, val);
		gall->SetPointError(centralityId, 0.0, error);
	}

	/// Stack graphs into array
	mGraph.push_back( gall );
	mGraphDraw.push_back( gdraw );

	return 1 ;
}

//____________________________________________________________________________________________________
Double_t StGlauberPlotMaker::GetYMinimum() const
{
	Double_t ymin = 0.0 ;
	if( mName.CompareTo("impactparameter", TString::kIgnoreCase) == 0 )    ymin = 0.0 ;
	else if ( mName.CompareTo("npart", TString::kIgnoreCase) == 0 )        ymin = 0.0 ;
	else if ( mName.CompareTo("ncoll", TString::kIgnoreCase) == 0 )        ymin = 0.0 ;
	else if ( mName.CompareTo("multiplicity", TString::kIgnoreCase) == 0 ) ymin = 0.0 ;
	else if ( mName.CompareTo("arearp", TString::kIgnoreCase) == 0 )       ymin = 0.0 ;
	else if ( mName.CompareTo("areapp", TString::kIgnoreCase) == 0 )       ymin = 0.0 ;
	else if ( mName.CompareTo("eccrp", TString::kIgnoreCase) == 0 )        ymin = 0.0 ;
	else if ( mName.CompareTo("eccrpm", TString::kIgnoreCase) == 0 )       ymin = 0.0 ;
	else if ( mName.CompareTo("eccpp_0", TString::kIgnoreCase) == 0 )      ymin = 0.0 ;
	else if ( mName.CompareTo("eccpp_0_2", TString::kIgnoreCase) == 0 )    ymin = 0.0 ;
	else if ( mName.CompareTo("eccpp_1", TString::kIgnoreCase) == 0 )      ymin = 0.0 ;
	else if ( mName.CompareTo("eccpp_1_2", TString::kIgnoreCase) == 0 )    ymin = 0.0 ;
	else if ( mName.CompareTo("eccppm_0", TString::kIgnoreCase) == 0 )     ymin = 0.0 ;
	else if ( mName.CompareTo("eccppm_0_2", TString::kIgnoreCase) == 0 )   ymin = 0.0 ;
	else if ( mName.CompareTo("eccppm_1", TString::kIgnoreCase) == 0 )     ymin = 0.0 ;
	else if ( mName.CompareTo("eccppm_1_2", TString::kIgnoreCase) == 0 )   ymin = 0.0 ;
	else{
		Error("StGlauberPlotMaker::GetYMinimum", Form("Cannot find %s in the list. return 0", mName.Data()));
		return 0.0 ;
	}

	return ymin ;
}

//____________________________________________________________________________________________________
Double_t StGlauberPlotMaker::GetYMaximum() const
{
	Double_t ymax = 0.0 ;
	if( mName.CompareTo("impactparameter", TString::kIgnoreCase) == 0 )    ymax = 18.0 ;
	else if ( mName.CompareTo("npart", TString::kIgnoreCase) == 0 )        ymax = 420.0 ;
	else if ( mName.CompareTo("ncoll", TString::kIgnoreCase) == 0 )        ymax = 1200.0 ;
	else if ( mName.CompareTo("multiplicity", TString::kIgnoreCase) == 0 ) ymax = 1200.0 ;
	else if ( mName.CompareTo("arearp", TString::kIgnoreCase) == 0 )       ymax = 42.0 ;
	else if ( mName.CompareTo("areapp", TString::kIgnoreCase) == 0 )       ymax = 42.0 ;
	else if ( mName.CompareTo("eccrp", TString::kIgnoreCase) == 0 )        ymax = 0.98 ;
	else if ( mName.CompareTo("eccrpm", TString::kIgnoreCase) == 0 )       ymax = 0.98 ;
	else if ( mName.CompareTo("eccpp_0", TString::kIgnoreCase) == 0 )      ymax = 0.98 ;
	else if ( mName.CompareTo("eccpp_0_2", TString::kIgnoreCase) == 0 )    ymax = 0.98 ;
	else if ( mName.CompareTo("eccpp_1", TString::kIgnoreCase) == 0 )      ymax = 0.98 ;
	else if ( mName.CompareTo("eccpp_1_2", TString::kIgnoreCase) == 0 )    ymax = 0.98 ;
	else if ( mName.CompareTo("eccppm_0", TString::kIgnoreCase) == 0 )     ymax = 0.98 ;
	else if ( mName.CompareTo("eccppm_0_2", TString::kIgnoreCase) == 0 )   ymax = 0.98 ;
	else if ( mName.CompareTo("eccppm_1", TString::kIgnoreCase) == 0 )     ymax = 0.98 ;
	else if ( mName.CompareTo("eccppm_1_2", TString::kIgnoreCase) == 0 )   ymax = 0.98 ;
	else{
		Error("StGlauberPlotMaker::GetYMaximum", Form("Cannot find %s in the list. return 0", mName.Data()));
		return 0.0 ;
	}

	return ymax ;
}

//____________________________________________________________________________________________________
TString StGlauberPlotMaker::GetYTitle() const
{
	TString title("");

	if( mName.CompareTo("impactparameter", TString::kIgnoreCase) == 0 )    title = "#LTb#GT (fm)" ;
	else if ( mName.CompareTo("npart", TString::kIgnoreCase) == 0 )        title = "N_{part}" ;
	else if ( mName.CompareTo("ncoll", TString::kIgnoreCase) == 0 )        title = "N_{coll}" ;
	else if ( mName.CompareTo("multiplicity", TString::kIgnoreCase) == 0 ) title = "Multiplicity" ;
	else if ( mName.CompareTo("arearp", TString::kIgnoreCase) == 0 )       title = "#LTS_{RP}#GT (fm^{2})" ;
	else if ( mName.CompareTo("areapp", TString::kIgnoreCase) == 0 )       title = "#LTS_{PP}#GT (fm^{2})" ;
	else if ( mName.CompareTo("eccrp", TString::kIgnoreCase) == 0 )        title = "#LT#varepsilon_{RP}#GT" ;
	else if ( mName.CompareTo("eccrpm", TString::kIgnoreCase) == 0 )       title = "#LT#varepsilon_{RP}#GT" ;
	else if ( mName.CompareTo("eccpp_0", TString::kIgnoreCase) == 0 )      title = "#LT#varepsilon_{part}#GT" ;
	else if ( mName.CompareTo("eccpp_0_2", TString::kIgnoreCase) == 0 )    title = "#varepsilon_{part}{2}" ;
	else if ( mName.CompareTo("eccpp_1", TString::kIgnoreCase) == 0 )      title = "#LT#varepsilon_{3,part}#GT" ;
	else if ( mName.CompareTo("eccpp_1_2", TString::kIgnoreCase) == 0 )    title = "#varepsilon_{3,part}{2}" ;
	else if ( mName.CompareTo("eccppm_0", TString::kIgnoreCase) == 0 )     title = "#LT#varepsilon_{part}#GT" ;
	else if ( mName.CompareTo("eccppm_0_2", TString::kIgnoreCase) == 0 )   title = "#varepsilon_{part}{2}" ;
	else if ( mName.CompareTo("eccppm_1", TString::kIgnoreCase) == 0 )     title = "#LT#varepsilon_{3,part}#GT" ;
	else if ( mName.CompareTo("eccppm_1_2", TString::kIgnoreCase) == 0 )   title = "#varepsilon_{3,part}{2}" ;
	else{
		Error("StGlauberPlotMaker::GetYTitle", Form("Cannot find %s in the list. return 0", mName.Data()));
		return "";
	}

	return title ;
}

//____________________________________________________________________________________________________
TGraphErrors* StGlauberPlotMaker::Divide(const TGraphErrors& g0, const TGraphErrors& g1) const
{
	TGraphErrors* g = new TGraphErrors();
	g->SetMarkerSize( g0.GetMarkerSize() );
	g->SetMarkerStyle( g0.GetMarkerStyle() );
	g->SetMarkerColor( g0.GetMarkerColor() );
	g->SetLineColor( g0.GetLineColor() );

	for(Int_t i=0;i<g0.GetN();i++){
		const Double_t y0  = g0.GetY()[i] ;
		const Double_t y1  = g1.GetY()[i] ;
		const Double_t y0e = g0.GetEY()[i] ;
		const Double_t y1e = g1.GetEY()[i] ;

		if( y1 != 0.0 ){
			const Double_t r  = y0/y1 ;
			const Double_t re = TMath::Abs(r) * TMath::Sqrt(TMath::Power(y0e/y0,2.0)+TMath::Power(y1e/y1,2.0)) ;
			const Int_t n = g->GetN() ;

			g->SetPoint(n, g0.GetX()[i], r) ;
			g->SetPointError(n, g0.GetEX()[i], re) ;
		}
	}

	return g ;
}

//____________________________________________________________________________________________________
TGraphErrors* StGlauberPlotMaker::SystematicErrors(const UInt_t mode)
{
	/// Require at least two graphs
	if( mGraph.size() < 2 ){
		Error("StGlauberPlotMaker::SystematicErrors", "Number of graphs < 2, at least 2 graphs are needed to evaluate sys. errors. abort");
		assert(0);
	}

	/// Evaluate systematic errors, and write tables
	LOG_INFO << "StGlauberPlotMaker::SystematicErrors  Start evaluating systematic errors" << endm;

	/// Take quadratic sum of the difference between default and all other available data points
	///   assume first graph contains default data points 
	const UInt_t nCent = mGraph[0]->GetN() ;
	Double_t qSum[nCent] ;
	for(UInt_t ic=0; ic<nCent; ic++){
		qSum[ic] = 0.0 ;
	}

	const UInt_t nSize = mGraph.size() ;
	for(UInt_t id=1; id<nSize; id++){
		LOG_INFO << "StGlauberPlotMaker::SystematicErrors Use " << mGraph[id]->GetTitle() << endm;

		for(UInt_t ic=0; ic<nCent; ic++){
			const Double_t val  = mGraph[id]->GetY()[ic];
			const Double_t ref  = mGraph[0]->GetY()[ic];
			const Double_t diff = val - ref;
			qSum[ic] += diff * diff ;
		}
	}

	// qSum = sqrt(sum{diff})
	mSystematicError = new TGraph();

	Double_t factor = 1.0/TMath::Sqrt(12.0) ;
	if( mode == 1 ) factor = 1.0 ;

	for(UInt_t ic=0; ic<nCent; ic++){
		qSum[ic] = TMath::Sqrt(qSum[ic]) * factor ;
		mSystematicError->SetPoint(ic, ic+0.5, qSum[ic]);
	}

	// Fill graph
	TGraphErrors* g = new TGraphErrors() ;
	g->SetFillColor(kYellow);
	g->SetLineColor(kYellow);

	for(UInt_t ic=0; ic<mNCentrality; ic++){
		g->SetPoint(ic, mGraph[0]->GetX()[ic], mGraph[0]->GetY()[ic]);
		g->SetPointError(ic, 0.0, qSum[ic]);
	}

	return g ;
}

//____________________________________________________________________________________________________
void StGlauberPlotMaker::Draw(const UInt_t mode)
{
	/// Draw graphs and evaluate systematic errors
	TColor::CreateColorWheel();
	gStyle->SetPadRightMargin(0.05);

	/// Systematic errors
	TGraphErrors* gSysError = SystematicErrors(mode) ;

	/// Draw
	TCanvas* c1 = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 600, 800);
	c1->Divide(1, 2);

	// Styles, and colors
	const UInt_t style[] = {20, 21, 25, 22, 26, 29, 21, 25, 22, 26, 27, 28};
	const UInt_t color[] = {kBlack, kRed, kRed, kBlue, kBlue, kMagenta+1,
		kGreen+2, kGreen+2,
		kOrange+1, kOrange+1,
		kCyan-3, kCyan-3
	};

	//----------------------------------------------------------------------------------------------------
	// Draw
	//----------------------------------------------------------------------------------------------------
	c1->cd(1);

	/// Minimum/Maximum according to the mName
	const Double_t ymin = GetYMinimum() ;
	const Double_t ymax = GetYMaximum() ;

	TH1* frame = c1->GetPad(1)->DrawFrame(0, ymin, 80, ymax);
	frame->SetXTitle("% Most central");
	frame->SetYTitle(GetYTitle());

	TLegend* leg = new TLegend(0.05, 0.1, 0.95, 0.9);
	leg->SetTextSize(0.05);
	leg->SetFillColor(10);
	//  leg->SetBorderSize(1);

	const UInt_t nSize = mGraphDraw.size() ;
	TGraphErrors* gRatio[nSize] ;
	for(UInt_t id=0; id<nSize; id++){
		mGraphDraw[id]->SetMarkerSize(1.2);
		mGraphDraw[id]->SetMarkerStyle(style[id]);
		mGraphDraw[id]->SetMarkerColor(color[id]);
		mGraphDraw[id]->SetLineColor(color[id]);
		mGraphDraw[id]->Draw("P");

		// Calculate ratio
		gRatio[id] = Divide(*mGraphDraw[id], *mGraphDraw[0]) ;

		// Add entry in legend
		leg->AddEntry(mGraphDraw[id], gRatio[id]->GetTitle(), "P");
	}
	mGraphDraw[0]->Draw("P");

	//----------------------------------------------------------------------------------------------------
	// Draw ratio
	//----------------------------------------------------------------------------------------------------
	TPad* pad = (TPad*) c1->GetPad(2);
	pad->Divide(2, 1);
	pad->cd(1);

	const Double_t rmin = 1.0 - 0.3 ;
	const Double_t rmax = 1.0 + 0.3 ;
	TH1* frameRatio = pad->GetPad(1)->DrawFrame(0, rmin, 80, rmax);
	frameRatio->SetXTitle("% Most central");
	frameRatio->SetYTitle(Form("Ratio of %s", GetYTitle().Data()));

	TLine* lzero = new TLine(0, 1.0, 80, 1.0);
	lzero->SetLineStyle(3);
	lzero->Draw() ;

	for(UInt_t id=1; id<nSize; id++){
		gRatio[id]->Draw("P");
	}

	//----------------------------------------------------------------------------------------------------
	// Draw legend
	//----------------------------------------------------------------------------------------------------
	pad->cd(2);
	leg->Draw() ;

	c1->cd();
	c1->Update();

	c1->Print(Form("figure/systematicerror_%s.eps", mName.Data()));
	//  c1->Print(Form("%s_systematic_error.png", mName.Data()));

	//----------------------------------------------------------------------------------------------------
	// Draw default vs centrality with systematic error (eps)
	//----------------------------------------------------------------------------------------------------
	TCanvas* c2 = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++));
	TH1* frame2 = c2->DrawFrame(0, ymin, 80, ymax);
	frame2->SetXTitle("% Most central");
	frame2->SetYTitle(GetYTitle());

	gSysError->Draw("E3");
	mGraphDraw[0]->Draw("P");

	c2->cd();
	c2->Update();

	c2->Print(Form("figure/%s_vs_centrality_with_systematicerror.eps", mName.Data()));

	//----------------------------------------------------------------------------------------------------
	// Draw default vs centrality with systematic error (png for web, small TCanvas)
	//----------------------------------------------------------------------------------------------------
	TCanvas* c3 = new TCanvas(Form("c%d", mCanvasId), Form("c%d", mCanvasId++), 700*0.5, 500*0.5);
	TH1* frame3 = c3->DrawFrame(0, ymin, 80, ymax);
	frame3->SetXTitle("% Most central");
	frame3->SetYTitle(GetYTitle());

	gSysError->Draw("E3");
	mGraphDraw[0]->Draw("P");

	c3->cd();
	c3->Update();

	c3->Print(Form("figure/%s_vs_centrality_with_systematicerror.png", mName.Data()));

	//----------------------------------------------------------------------------------------------------
	// Write table
	//----------------------------------------------------------------------------------------------------
	const TString tableName(Form("table_%s_vs_centrality_systematicerror.txt", mName.Data()));
	ofstream fout(tableName.Data()) ;
	LOG_INFO << Form("StGlauberPlotMaker::Draw  Write table %s in the current directory", tableName.Data()) << endm;

	for(Int_t ic=0; ic<mGraph[0]->GetN(); ic++){
		const Double_t val = mGraph[0]->GetY()[ic] ;
		const Double_t sys = mSystematicError->GetY()[ic] ;
		const Double_t centMin = StGlauberConstUtilities::GetCentralityMin(ic) ;
		const Double_t centMax = StGlauberConstUtilities::GetCentralityMax(ic) ;

		fout << Form("%10d    %1.1f      %1.1f        %1.5f        %1.5f",
				ic, centMin, centMax, val, sys) << endl;
	}
	fout << endl << endl ;
	fout << "# <centrality bin>  <minimum centrality>  <maximum centrality>  <value>  <sys. error>" << endl;
}


