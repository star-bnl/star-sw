
void CombineHists(const char* infile1, const char* infile2, const char* outfile)
{
    cout <<"CombineHists()"<<endl;

    cout <<"Open read file:\t"<<infile1<<endl;
    TFile* if1 = new TFile(infile1,"READ");
    TH1* pp1 = plusPlus;
    TH1* mm1 = minusMinus;
    TH1* pm1 = plusMinus;

    cout <<"Open read file:\t"<<infile2<<endl;
    TFile* if2 = new TFile(infile2,"READ");
    TH1* pp2 = plusPlus;
    TH1* mm2 = minusMinus;
    TH1* pm2 = plusMinus;

    cout <<"Add histograms"<<endl;
    cout <<"Size before adding:\t"<<pp1->GetEntries()<<"\t"<<mm1->GetEntries()<<"\t"<<pm1->GetEntries()<<endl;
    pp1->Add(pp2);
    mm1->Add(mm2);
    pm1->Add(pm2);
    cout <<"Size after adding:\t"<<pp1->GetEntries()<<"\t"<<mm1->GetEntries()<<"\t"<<pm1->GetEntries()<<endl;

    cout <<"Open write file:\t"<<outfile<<endl;
    TFile* of = new TFile(outfile,"RECREATE");
    of->cd();
    pp1.Write();
    mm1.Write();
    pm1.Write();
    of->Write();
    of->Close();
}
