
#include <iostream>

void Signal(const char* infile)
{
    cout <<"Opend read file:\t"<<infile<<endl;
    TFile* ifile = new TFile(infile,"READ");

    TH1* pp = plusPlus;
    pp->SetLineWidth(2);
    pp->SetXTitle("M (GeV)");

    TH1* mm = minusMinus;
    mm->SetLineWidth(2);
    mm->SetXTitle("M (GeV)");

    TH1* pm = plusMinus;
    pm->SetLineWidth(2);
    pm->SetXTitle("M (GeV)");

    TH1* ls = likeSign;
    ls->SetLineWidth(2);
    ls->SetXTitle("M (GeV)");

    TH1* sub = subtracted;
    sub->SetLineWidth(2);
    sub->SetXTitle("M (GeV)");

    TH1* seff = seff;
    seff->SetLineWidth(2);
    seff->SetXTitle("M (GeV)");
    
    c1 = new TCanvas();
    c1->SetLogy();
    pm->SetTitle("Invariant Mass: black=++, red=--, blue=+-, green=2.*sqrt(++*--)");
    pm->SetLineColor(4);
    pm->Draw("pe");
    //pp->Draw("pesame");
    //mm->SetLineColor(2);
    //mm->Draw("pesame");
    ls->SetLineColor(3);
    ls->Draw("pesame");
    
    c2 = new TCanvas();
    sub->SetTitle("Background Subtracted: (+-) - 2*sqrt(++*--)");
    sub->Draw("pe");

    c3 = new TCanvas();
    TH2* zone = new TH2D("zone",seff->GetTitle(),1, 6, 14., 1, -10., 10.);
    zone->Draw();
    seff->Draw("pesame");

}
