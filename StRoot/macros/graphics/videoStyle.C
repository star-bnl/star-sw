///////////////////////////////////////////////////////////////////////////////
//
// $Id: videoStyle.C,v 1.3 2000/02/04 16:42:46 posk Exp $
//
// Author: Art Poskanzer, LBNL, Oct. 1999
// Description:  Style file for video presentation histograms.
//               Just execute this macro before making your graphs.
//               Begining with ROOT 2.23/11 you can do
//                    gROOT->SetStyle("Video");
//                    gROOT->ForceStyle();               
//               You can return to defaultStyle with 
//                    gROOT->SetStyle("Default");
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: videoStyle.C,v $
// Revision 1.3  2000/02/04 16:42:46  posk
// Documented the availability in ROOT.
//
// Revision 1.2  1999/12/22 21:43:58  posk
// Reduced the line widths to compensate for the increase in postscript
// line width which started in ROOT 2.23/08.
//
// Revision 1.1  1999/10/28 21:28:07  posk
// Style file for making histograms for video presentations.
//
//
///////////////////////////////////////////////////////////////////////////////

//  A graph made with the ROOT defaultStyle:
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/defaultStyle.gif"> </P> End_Html //
// 
//  The same graph made with this videoStyle:
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/videoStyle.gif"> </P> End_Html // 

{
TStyle* myStyle = new  TStyle("videoStyle", "Video Style");

myStyle->SetPalette(1,0);
myStyle->SetCanvasColor(10);
myStyle->SetCanvasBorderMode(0);
myStyle->SetLineWidth(3);
myStyle->SetFrameLineWidth(3);
myStyle->SetFrameFillColor(10);
myStyle->SetPadColor(10);
myStyle->SetPadTickX(1);
myStyle->SetPadTickY(1);
myStyle->SetPadBottomMargin(0.2);
myStyle->SetPadLeftMargin(0.2);
myStyle->SetHistLineWidth(8);
myStyle->SetHistLineColor(kRed);
myStyle->SetLabelSize(0.06,"X");
myStyle->SetLabelSize(0.06,"Y");
//myStyle->SetLabelOffset(0.01,"X");
//myStyle->SetLabelOffset(0.01,"Y");
myStyle->SetLabelColor(kBlue,"X");
myStyle->SetLabelColor(kBlue,"Y");
myStyle->SetTitleSize(0.08,"X");
myStyle->SetTitleSize(0.08,"Y");
//myStyle->SetTitleOffset(0.,"X");
//myStyle->SetTitleOffset(0.,"Y");
myStyle->SetTitleColor(10);
myStyle->SetTitleTextColor(kBlue);
//myStyle->SetOptStat(1110111);
myStyle->SetStatColor(10);
myStyle->SetFuncWidth(8);
myStyle->SetFuncColor(kGreen);

gROOT->ForceStyle();
gStyle->ls();  
}
