/*******************************************************
 * $Id: StRichViewer.cxx,v 1.1 2000/01/18 21:32:04 lasiuk Exp $
 *
 * Description:
 *  Implementation of the Viewer displaying module
 *
 ********************************************************
 * $Log: StRichViewer.cxx,v $
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 ********************************************************/
#include "StRichViewer.h"

#ifdef RICH_WITH_VIEWER
#include <iostream.h>

#include "TROOT.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"

#ifndef ST_NO_NAMESPACES
#include "StRichViewer.h"

    StRichViewer* StRichViewer::p2View = 0;
    
    int StRichViewer::histograms = 0;
//namespace StRichRawData {

    StRichViewer::StRichViewer() 
    {
	//
	//  creates a Canvas, histograms.
	//  Sets attributes. Graphical stuff.
	//
  
	
	StRichViewer::p2View = this;   // access to current instance
	
	mCanvas1    = new TCanvas("mCanvas1","Rich Digitalization",700,500);
	mHFile = (TFile*)gROOT->FindObject("hsimple.root");
	if (mHFile)
	    mHFile->Close();
	mHFile = new TFile("hsimple.root","RECREATE","Rich Digitalization");
	
	mParticleId          = new TH1F("mParticleId","Particle Identification",
					100,0,30);
	mWhichQuadrant       = new TH1F("mWhichQuadrant","Quadrant number",
					100,0,5);
	mClusterElectrons    = new TH1F("mClusterElectrons","electrons per collision",
					100,0,21);
	mErrorDetection      = new TH1F("mErrorDetection","Detected Errors",
					100,0,100);                                   
	mWires               = new TH1F("mWires","Charges on Wires",
					100,-20,220);
	mWhichWire           = new TH1F("mWhichWire","Wire number",
					100,0,195);
	mFeedback            = new TH1F("mFeedback","Feedback photons",
					40,0,10);
	mPolia               = new TH1F("mPolia","Polia Distribution",
					100,0,2);
	mNoise               = new TH1F("mNoise", "Electric Noise", 
					100, -5000.0, 5000.0);
	mTotalCharge         = new TH1F("mTotalCharge","Total charge on pads",
					100,0.0,1.0);
	mADCSignal           = new TH2F("mADCSignal","ADC Signal on pads",
					96,0,96,160,0,160);              
	mAnalogSignals       = new TH2F("mAnalogSignals","Analog Signals", 
					50,-66.0,66.6,50,-42.0,42.0);
	mPadPlane            = new TH2F("mPadPlane","Particle distibution in RICH",
					50,-66.0,66.0,50,-42.0,42.0);
	cout << "Set Options " << endl;
	mCanvas1->SetFillColor(42);
	//mCanvas1->GetFrame()->SetFillColor(21);
	//mCanvas1->GetFrame()->SetBorderSize(6);
	//mCanvas1->GetFrame()->SetBorderMode(-1); 
	
	mParticleId->SetFillColor(0);
	mWhichQuadrant->SetFillColor(0);
	mClusterElectrons->SetFillColor(0);
	mErrorDetection->SetFillColor(0);
	mWires->SetFillColor(0);
	mWhichWire->SetFillColor(0);
	mFeedback->SetFillColor(0);
	mPolia->SetFillColor(0);
	mNoise->SetFillColor(0);
	mTotalCharge->SetFillColor(0);
	mADCSignal->SetFillColor(0);
	mAnalogSignals->SetFillColor(0);
	mPadPlane->SetFillColor(0);
	
    }

int StRichViewer::foo = 0;
    StRichViewer::~StRichViewer()
    {
	delete mCanvas1;
	delete mHFile;
    //  Sets attributes. Graphical stuff.
	delete 	mParticleId ; 
	delete  mWhichQuadrant; 
	delete 	mClusterElectrons;
	delete 	mErrorDetection ; 
	delete 	mWires ;
	delete 	mWhichWire;  
	delete	mFeedback;        
	delete	mPolia;          
	delete	mNoise;          
	delete	mTotalCharge; 
	delete 	mADCSignal;      
	delete	mAnalogSignals;  
	delete	mPadPlane;       
	
    }
    
    mHFile->Write();
    void StRichViewer::update()
    {
	//
	//  Updates the view. Needed after each Draw().
	//

	//      mParticleId->Draw() ; 
	//      mWhichQuadrant->Draw(); 
	//      mClusterElectrons->Draw();
	//      mErrorDetection->Draw() ; 
	//      mWires->Draw() ;
	// 	mWhichWire->Draw();  
	//	mFeedback->Draw();        
	//	mPolia->Draw();         
	//	mNoise->Draw();         
	//	mTotalCharge->Draw();
	// 	mADCSignal->Draw();    
	//	mAnalogSignals->Draw();
	//	mPadPlane->Draw();    
	
	mCanvas1->Modified();
	mCanvas1->Update();
	
	mHFile->Write();
    }
	p2View = new StRichViewer();

    StRichViewer* StRichViewer::getView()
    {
	return p2View;
    }
    
    return p2View;
}

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
