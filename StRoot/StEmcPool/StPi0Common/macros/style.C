#if !defined(__CINT__) || defined(__MAKECINT__) 

#include <TStyle.h> 
#include <TAttFill.h> 
#include <TColor.h> 
#include <TTF.h>

#endif 

void style() {
	cout << "Applying my style..." << endl;
        gStyle->SetPalette(1);
        gStyle->SetOptStat(0);
        //gStyle->SetFillColor(kWhite);
        gStyle->SetCanvasColor(kWhite);
        gStyle->SetHistFillColor(kWhite);
        gStyle->SetTitleFillColor(kWhite);
        gStyle->SetHistLineColor(kBlack);
        gStyle->SetFrameFillColor(kWhite);
        gStyle->SetFrameLineColor(kBlack);
        gStyle->SetFrameBorderSize(1);
        gStyle->SetFrameBorderMode(0);
        gStyle->SetPadColor(kWhite);
        gStyle->SetStatColor(kWhite);
        gStyle->SetLabelFont(132, "xyz");
        gStyle->SetStatFont(132);
        gStyle->SetTitleFont(132, "xyz");
        gStyle->SetTitleFont(22, "title");
        gStyle->SetTextFont(132);
        //gStyle->SetTitleOffset(1.3, "y");

        gStyle->SetPadLeftMargin(0.13);
        gStyle->SetPadRightMargin(0.07);
        gStyle->SetPadTopMargin(0.07);
        gStyle->SetPadBottomMargin(0.13);

	//gStyle->SetEndErrorSize(5);

	//TTF::SetSmoothing(kFALSE); // default is kTRUE
	//TTF::SetKerning(kFALSE); // default is kTRUE
	//TTF::SetHinting(kTRUE); // default is kFALSE
}
