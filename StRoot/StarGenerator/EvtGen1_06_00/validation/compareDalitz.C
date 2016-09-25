void compareDalitz() {

  compareDalitzPlot("rootFiles/DDalitzMode1.root", "rootFiles/XmlDalitzMode1.root", "Mode1");
  compareDalitzPlot("rootFiles/DDalitzMode1b.root", "rootFiles/XmlDalitzMode1b.root", "Mode1b");

  compareDalitzPlot("rootFiles/DDalitzMode2.root", "rootFiles/XmlDalitzMode2.root", "Mode2");
  compareDalitzPlot("rootFiles/DDalitzMode2b.root", "rootFiles/XmlDalitzMode2b.root", "Mode2b");
  compareDalitzPlot("rootFiles/DDalitzMode2KS.root", "rootFiles/XmlDalitzMode2KS.root", "Mode2KS");
  compareDalitzPlot("rootFiles/DDalitzMode2KL.root", "rootFiles/XmlDalitzMode2KL.root", "Mode2KL");
  compareDalitzPlot("rootFiles/DDalitzMode2KSb.root", "rootFiles/XmlDalitzMode2KSb.root", "Mode2KSb");
  compareDalitzPlot("rootFiles/DDalitzMode2KLb.root", "rootFiles/XmlDalitzMode2KLb.root", "Mode2KLb");

  compareDalitzPlot("rootFiles/DDalitzMode3.root", "rootFiles/XmlDalitzMode3.root", "Mode3");
  compareDalitzPlot("rootFiles/DDalitzMode3b.root", "rootFiles/XmlDalitzMode3b.root", "Mode3b");
  compareDalitzPlot("rootFiles/DDalitzMode3KS.root", "rootFiles/XmlDalitzMode3KS.root", "Mode3KS");
  compareDalitzPlot("rootFiles/DDalitzMode3KL.root", "rootFiles/XmlDalitzMode3KL.root", "Mode3KL");
  compareDalitzPlot("rootFiles/DDalitzMode3KSb.root", "rootFiles/XmlDalitzMode3KSb.root", "Mode3KSb");
  compareDalitzPlot("rootFiles/DDalitzMode3KLb.root", "rootFiles/XmlDalitzMode3KLb.root", "Mode3KLb");

  compareDalitzPlot("rootFiles/DDalitzMode4.root", "rootFiles/XmlDalitzMode4.root", "Mode4");
  compareDalitzPlot("rootFiles/DDalitzMode4b.root", "rootFiles/XmlDalitzMode4b.root", "Mode4b");

  compareDalitzPlot("rootFiles/DDalitzMode5.root", "rootFiles/XmlDalitzMode5.root", "Mode5");
  compareDalitzPlot("rootFiles/DDalitzMode5b.root", "rootFiles/XmlDalitzMode5b.root", "Mode5b");
  compareDalitzPlot("rootFiles/DDalitzMode5KS.root", "rootFiles/XmlDalitzMode5KS.root", "Mode5KS");
  compareDalitzPlot("rootFiles/DDalitzMode5KL.root", "rootFiles/XmlDalitzMode5KL.root", "Mode5KL");
  compareDalitzPlot("rootFiles/DDalitzMode5KSb.root", "rootFiles/XmlDalitzMode5KSb.root", "Mode5KSb");
  compareDalitzPlot("rootFiles/DDalitzMode5KLb.root", "rootFiles/XmlDalitzMode5KLb.root", "Mode5KLb");

  compareDalitzPlot("rootFiles/DDalitzMode6.root", "rootFiles/XmlDalitzMode6.root", "Mode6");
  compareDalitzPlot("rootFiles/DDalitzMode6b.root", "rootFiles/XmlDalitzMode6b.root", "Mode6b");

  compareDalitzPlot("rootFiles/DDalitzMode7.root", "rootFiles/XmlDalitzMode7.root", "Mode7");
  compareDalitzPlot("rootFiles/DDalitzMode7b.root", "rootFiles/XmlDalitzMode7b.root", "Mode7b");

  compareDalitzPlot("rootFiles/DDalitzMode8.root", "rootFiles/XmlDalitzMode8.root", "Mode8");
  compareDalitzPlot("rootFiles/DDalitzMode8b.root", "rootFiles/XmlDalitzMode8b.root", "Mode8b");

  compareDalitzPlot("rootFiles/DDalitzMode9.root", "rootFiles/XmlDalitzMode9.root", "Mode9");
  compareDalitzPlot("rootFiles/DDalitzMode9b.root", "rootFiles/XmlDalitzMode9b.root", "Mode9b");

  compareDalitzPlot("rootFiles/DDalitzMode10.root", "rootFiles/XmlDalitzMode10.root", "Mode10");
  compareDalitzPlot("rootFiles/DDalitzMode10b.root", "rootFiles/XmlDalitzMode10b.root", "Mode10b");

  compareDalitzPlot("rootFiles/DDalitzMode11.root", "rootFiles/XmlDalitzMode11.root", "Mode11");
  compareDalitzPlot("rootFiles/DDalitzMode11b.root", "rootFiles/XmlDalitzMode11b.root", "Mode11b");

  compareDalitzPlot("rootFiles/DDalitzMode12.root", "rootFiles/XmlDalitzMode12.root", "Mode12");
  compareDalitzPlot("rootFiles/DDalitzMode12b.root", "rootFiles/XmlDalitzMode12b.root", "Mode12b");

}

void compareDalitzPlot(TString name1, TString name2, TString save="") {
  TCanvas c1;
  c1.Divide(2,2);
  TFile f1(name1);
  TFile f2(name2);
  TTree* t1 = (TTree*)f1.Get("dalitzTree");
  TTree* t2 = (TTree*)f2.Get("dalitzTree");
  t1->SetMarkerColor(kRed);
  t1->SetLineColor(kRed);
  t1->SetFillColor(kRed);
  t1->SetFillStyle(3004);
  t2->SetMarkerColor(kBlue);
  t2->SetLineColor(kBlue);
  t2->SetFillColor(kBlue);
  t2->SetFillStyle(3005);
  gStyle->SetOptStat(0);
  c1.cd(1);
  t1->Draw("invMass12Sq:invMass23Sq","");
  t2->Draw("invMass12Sq:invMass23Sq","","same");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetTitle("");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetXTitle("Mass^{2} 23 [GeV/c^{2}]^{2}");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetYTitle("Mass^{2} 12 [GeV/c^{2}]^{2}");
  c1.cd(2);
  t1->Draw("invMass12Sq","");
  t2->Draw("invMass12Sq","","same");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetTitle("");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetXTitle("Mass^{2} 12 [GeV/c^{2}]^{2}");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetYTitle("");
  c1.cd(3);
  t1->Draw("invMass23Sq","");
  t2->Draw("invMass23Sq","","same");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetTitle("");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetXTitle("Mass^{2} 23 [GeV/c^{2}]^{2}");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetYTitle("");
  c1.cd(4);
  t1->Draw("invMass13Sq","");
  t2->Draw("invMass13Sq","","same");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetTitle("");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetXTitle("Mass^{2} 13 [GeV/c^{2}]^{2}");
  ((TH1F*)gPad->GetPrimitive("htemp"))->SetYTitle("");
  c1.Update();

  if(save!="") {
    c1.SaveAs(save+".png");
  } else {
    cout <<"Hit Enter to continue"<<endl;
    while (getchar() != '\n');
  }
}
