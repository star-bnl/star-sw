///////////////////////////////////////////////////////////////////////////////
//
// $Id: boldStyle.C,v 1.3 2000/02/04 16:42:45 posk Exp $
//
// Author: Art Poskanzer, LBNL, Sep. 1999
// Description:  Style file for presentation histograms.
//               Just execute this macro before making your graphs.
//               Begining with ROOT 2.23/11 you can do
//                    gROOT->SetStyle("Bold");
//                    gROOT->ForceStyle();               
//               You can return to defaultStyle with 
//                    gROOT->SetStyle("Default");
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: boldStyle.C,v $
// Revision 1.3  2000/02/04 16:42:45  posk
// Documented the availability in ROOT.
//
// Revision 1.2  1999/12/22 21:43:56  posk
// Reduced the line widths to compensate for the increase in postscript
// line width which started in ROOT 2.23/08.
//
// Revision 1.1  1999/10/28 21:24:07  posk
// Style file for making presentation histograms.
//
//
///////////////////////////////////////////////////////////////////////////////

//  A graph made with the ROOT defaultStyle:
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/defaultStyle.gif"> </P> End_Html //
// 
//  The same graph made with this boldStyle:
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/boldStyle.gif"> </P> End_Html // 

{
TStyle* myStyle = new  TStyle("boldStyle", "Bold Style");

myStyle->SetPalette(1,0);
myStyle->SetCanvasColor(10);
myStyle->SetCanvasBorderMode(0);
myStyle->SetLineWidth(2);
myStyle->SetFrameLineWidth(2);
myStyle->SetFrameFillColor(10);
myStyle->SetPadColor(10);
myStyle->SetPadTickX(1);
myStyle->SetPadTickY(1);
myStyle->SetPadBottomMargin(0.15);
myStyle->SetPadLeftMargin(0.17);
myStyle->SetHistLineWidth(3);
myStyle->SetHistLineColor(kRed);
myStyle->SetLabelSize(0.04,"X");
myStyle->SetLabelSize(0.04,"Y");
myStyle->SetLabelSize(0.04,"Z");
//myStyle->SetLabelOffset(0.01,"X");
//myStyle->SetLabelOffset(0.01,"Y");
myStyle->SetLabelColor(kBlue,"X");
myStyle->SetLabelColor(kBlue,"Y");
myStyle->SetLabelColor(kBlue,"Z");
myStyle->SetTitleSize(0.06,"X");
myStyle->SetTitleSize(0.06,"Y");
myStyle->SetTitleSize(0.06,"Z");
//myStyle->SetTitleOffset(0.08,"X");
//myStyle->SetTitleOffset(0.08,"Y");
myStyle->SetTitleColor(10);
myStyle->SetTitleTextColor(kBlue);
//myStyle->SetOptStat(1110111);
myStyle->SetStatColor(10);
myStyle->SetFuncWidth(3);
myStyle->SetFuncColor(kGreen);

gROOT->ForceStyle();
gStyle->ls();  
}
