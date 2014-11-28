
//
// Create suitable canvas
// Plot everything on one page.

TCanvas *c1 = new TCanvas("c1","c1",1200,900);
gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);
c1->SetFillColor(0);
c1->SetFrameBorderSize(0);
c1->SetFrameBorderMode(0);
c1->Clear();
c1->Divide(4,3);


// Here are centrality and b/b_0 definitions.
double AuAu200GeV_11_xpos[] = {1.4, 1.68, 2.0, 2.38, 2.84, 3.33, 3.87, 4.46, 5.08, 5.54, 5.95};
double eAuAu200GeV_11_xpos[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
double AuAu200GeV_11_BbyB0[] = {0.05, 0.11, 0.16, 0.22, 0.28, 0.35, 0.43, 0.52, 0.63, 0.74, 0.85};

double AuAu62GeV_11_xpos[] = {1.4, 1.68, 2.0, 2.38, 2.84, 3.33, 3.87, 4.46, 5.08, 5.54, 5.95};
double eAuAu62GeV_11_xpos[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
double AuAu62GeV_11_BbyB0[] = {0.05, 0.11, 0.16, 0.22, 0.28, 0.35, 0.43, 0.52, 0.63, 0.74, 0.85};

double CuCu200GeV_11_xpos[] = { 1.53606, 1.86899, 2.25134, 2.67618, 2.99479, 3.16154, 3.33324, 3.49001, 3.69143, 3.89892};
double eCuCu200GeV_11_xpos[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
double CuCu200GeV_11_BbyB0[] = {  0.15, 0.22, 0.31, 0.41, 0.49, 0.54, 0.59, 0.64, 0.72, 0.83};

double CuCu62GeV_11_xpos[] = { 1.53606, 1.86899, 2.25134, 2.67618, 2.99479, 3.16154, 3.33324, 3.49001, 3.69143, 3.89892};
double eCuCu62GeV_11_xpos[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
double CuCu62GeV_11_BbyB0[] = {  0.15, 0.22, 0.31, 0.41, 0.49, 0.54, 0.59, 0.64, 0.72, 0.83};

// position is which gPad to use. -1 means don't draw parameter.
// margin is supposed to move y axis label so numbers can be seen.
// min and max are the minimum and y plotting limits.

int    ChiSquares_position = 4;
double ChiSquares_margin   = 0.13;
double ChiSquares_min   = 100;
double ChiSquares_max   = 500;
int    offset_position = 1;
double offset_margin   = 0.13;
double offset_min   = -0.5;
double offset_max   = 0.1;
int    cosPhi_position = 5;
double cosPhi_margin   = 0.10;
double cosPhi_min   = 0;
double cosPhi_max   = 0.6;
int    cos2Phi_position = 9;
double cos2Phi_margin   = 0.10;
double cos2Phi_min   = -0.1;
double cos2Phi_max   = 0.3;
int    MjAmp_position = 2;
double MjAmp_margin   = 0.10;
double MjAmp_min   = 0;
double MjAmp_max   = 1.5;
int    MjEta_position = 6;
double MjEta_margin   = 0.10;
double MjEta_min   = 0;
double MjEta_max   = 3.5;
int    MjPhi_position = 10;
double MjPhi_margin   = 0.10;
double MjPhi_min   = 0;
double MjPhi_max   = 3.5;
int    stringAmp_position = -1;
double stringAmp_margin   = 0.10;
double stringAmp_min   = 0;
double stringAmp_max   = 0.04;
int    stringEta_position = -1;
double stringEta_margin   = 0.10;
double stringEta_min   = 0;
double stringEta_max   = 1.5;
int    negAmp_position = -1;
double negAmp_margin   = 0.10;
double negAmp_min   = -0.08;
double negAmp_max   = 0;
int    negEta_position = -1;
double negEta_margin   = 0.10;
double negEta_min   = 0;
double negEta_max   = 1.5;
int    expAmp_position = 3;
double expAmp_margin   = 0.10;
double expAmp_min   = 0;
double expAmp_max   = 1;
int    expEta_position = 7;
double expEta_margin   = 0.10;
double expEta_min   = 0;
double expEta_max   = 0.6;
int    expPhi_position = 11;
double expPhi_margin   = 0.10;
double expPhi_min   = 0;
double expPhi_max   = 0.6;
int    jetOvercosphi_position = -1;
double jetOvercosphi_margin   = 0.10;
double jetOvercosphi_min   = 1;
double jetOvercosphi_max   = 3;
int    cos_etaD_position = -1;
double cos_etaD_margin   = 0.10;
double cos_etaD_min   = 1;
double cos_etaD_max   = 3;
int    cos_2etaD_position = -1;
double cos_2etaD_margin   = 0.10;
double cos_2etaD_min   = 1;
double cos_2etaD_max   = 3;
int    cos_3etaD_position = -1;
double cos_3etaD_margin   = 0.10;
double cos_3etaD_min   = 1;
double cos_3etaD_max   = 3;
int    cos_4etaD_position = -1;
double cos_4etaD_margin   = 0.10;
double cos_4etaD_min   = 1;
double cos_4etaD_max   = 3;


// Define colors, styles etc. for each data set
int   AuAu200GeV_11__color =  1;
int   AuAu200GeV_11__style = 20;
char *AuAu200GeV_11__mode = "ACP";

int   AuAu62GeV_11__color =  1;
int   AuAu62GeV_11__style = 24;
char *AuAu62GeV_11__mode = "CP";

int   CuCu200GeV_11__color =  1;
int   CuCu200GeV_11__style = 22;
char *CuCu200GeV_11__mode = "CP";

int   CuCu62GeV_11__color =  1;
int   CuCu62GeV_11__style = 26;
char *CuCu62GeV_11__mode = "CP";



// AuAu fits.
.x fitResults/cos_plotCode_AuAu200GeV_11c.C
.x fitResults/cos_plotCode_AuAu62GeV_11c.C

// CuCu fits.
.x fitResults/cos_plotCode_CuCu200GeV_11c.C
.x fitResults/cos_plotCode_CuCu62GeV_11c.C

//
// Here are binary scaling lines for minijet amplitudes.
// Tried taking them from the paper draft, but had problems finding x_max.

double AuAu200GeV_xpos[] = {1.4, 1.68, 2.0, 2.38, 2.84, 3.33, 3.87, 4.46, 5.08, 5.54, 5.95};
double AuAu200GeVBinaryScaling[] = {0.07989651499010804, 0.09277066208801726, 0.10657193605683837, 0.12186562965956817, 0.13902740863353683, 0.1559494480263731, 0.173263876556129, 0.1908819487877142, 0.20822311899650667, 0.22046042536393212, 0.23099470070980724};
TGraph *gAuAu200GeVBinaryScaling = new TGraph(11,AuAu200GeV_xpos,AuAu200GeVBinaryScaling);
gAuAu200GeVBinaryScaling->SetLineColor(1);
gAuAu200GeVBinaryScaling->SetLineStyle(7);
gAuAu200GeVBinaryScaling->SetMarkerColor(1);

double AuAu62GeV_xpos[] = {1.4, 1.68, 2.0, 2.38, 2.84, 3.33, 3.87, 4.46, 5.08, 5.54, 5.95};
double AuAu62GeVBinaryScaling[] = {0.054087467161180645, 0.06343510136022981, 0.07366482504604051, 0.0852601653044146, 0.098606180802071, 0.11212268443929063, 0.12633100486031612, 0.1411935732231137, 0.15623385378479587, 0.16709979899764252, 0.17662468738172454};
TGraph *gAuAu62GeVBinaryScaling = new TGraph(11,AuAu62GeV_xpos,AuAu62GeVBinaryScaling);
gAuAu62GeVBinaryScaling->SetLineColor(46);
gAuAu62GeVBinaryScaling->SetLineStyle(7);
gAuAu62GeVBinaryScaling->SetMarkerColor(46);

// Lines to indicate limits of nu.
double minNuAuAu[] = {1.25, 1.25};
double ampRangeAuAu[] = {0.0, 1.0};
TGraph *gminnuAuAu = new TGraph(2,minNuAuAu,ampRangeAuAu);
gminnuAuAu->SetLineColor(17);
gminnuAuAu->SetLineWidth(10);
gminnuAuAu->SetLineStyle(3);

double maxNuAuAu[] = {6.1, 6.1};
TGraph *gmaxnuAuAu = new TGraph(2,maxNuAuAu,ampRangeAuAu);
gmaxnuAuAu->SetLineColor(17);
gmaxnuAuAu->SetLineWidth(10);
gmaxnuAuAu->SetLineStyle(3);

c1->cd(MjAmp_position);
gAuAu200GeVBinaryScaling->Draw("CP");
gAuAu62GeVBinaryScaling->Draw("CP");
gminnuAuAu->Draw("CP");
gmaxnuAuAu->Draw("CP");

// Line to indicate eta_Delta/phi_Delta width if nothing is happening
double nuRange[] = {1.25, 6.1};
double angularWidth[] = {0.675, 0.675};
TGraph *gangularWidth = new TGraph(2,nuRange,angularWidth);
gangularWidth->SetLineColor(17);
gangularWidth->SetLineWidth(10);
gangularWidth->SetLineStyle(3);

c1->cd(MjEta_position);
gangularWidth->Draw("CP");
c1->cd(MjPhi_position);
gangularWidth->Draw("CP");


c1->Print("cosAmpFits.gif");
