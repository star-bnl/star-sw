//
// $Id: RunCorrectionHistos.C,v 1.2 2003/06/19 21:25:19 calderon Exp $
//
// This macro is the 3d step in the production of the Evaluation Histograms
// It does the division of the raw distributions (obtained in step 1, RunEvaluationHistos.C)
// once all files have been accumulated          (obtained in step 2, RunAdditionOfHistos.C)
//
// $Log: RunCorrectionHistos.C,v $
// Revision 1.2  2003/06/19 21:25:19  calderon
// Better handling of file names, don't assume the input files, but use
// options in the macros (keep setting the default values as before).
// Adding Zbigniew's scripts and macros.
// Modified FillHistos.C due to a bug in the handling of the file name,
// which was fortuitously counterbalanced by a call to TString::Data()
// which truncated the TString at just the right place.
//
// Revision 1.1  2002/11/27 00:09:29  calderon
// New version of evaluator using the minimctrees
// Macros for running the evaluation and perl script to coordinate
//

void errorsFrom2d(TH2D* table, TH1D* proj) {
    for (int iY=1; iY!=table->GetYaxis()->GetNbins()+1;++iY) {
	proj->SetBinError(iY,table->GetCellError(11,iY));
    }
}
void histStyle(TH1D* hist, int linecolor=4) {
    hist->SetLineColor(linecolor);
    hist->GetYaxis()->SetNdivisions(510);
    hist->SetXTitle("pt (GeV/c)");
    hist->SetMarkerStyle(20);
    hist->SetMarkerColor(linecolor);
    hist->GetXaxis()->SetTitleOffset(0.8);
    hist->GetXaxis()->SetLabelOffset(0.0005);
    hist->GetXaxis()->SetLabelSize(0.03);

//     TString title = hist->GetTitle();
//     title += " Embedding High Set 1";
//     hist->SetTitle(title.Data());
}
void RunCorrectionHistos(bool fromEmbedding = false,
			 TString topDir="StiEvalOutputRawHistos/",
			 TString fileName = "SumHistosEvalItTest.root") {
    //
    // on rcf
    // file: StiEvalOutputRawHistos/SumHistosEvalItTest.root
    fileName.Prepend(topDir);
    TFile* iofile = TFile::Open(fileName.Data(),"UPDATE");
    gStyle->SetPalette(1,0);
    gStyle->SetTitleH(.05);
    gStyle->SetTitleW(0.6);
    gStyle->SetOptStat(0);
    gStyle->SetTitleOffset(.9, "x");
    gStyle->SetTitleOffset(.65, "y");

    //
    // get the histograms
    //
    
    TH2D* rawHistos[10][18]; // name, suffix
    TString suffixes[18];
    int si = 0;
    suffixes[si++]="Hi08"; suffixes[si++]="Me08"; suffixes[si++]="Lo08";
    suffixes[si++]="Hi09"; suffixes[si++]="Me09"; suffixes[si++]="Lo09";
    suffixes[si++]="Hi11"; suffixes[si++]="Me11"; suffixes[si++]="Lo11";
    suffixes[si++]="Hi12"; suffixes[si++]="Me12"; suffixes[si++]="Lo12";
    suffixes[si++]="Hi14"; suffixes[si++]="Me14"; suffixes[si++]="Lo14";
    suffixes[si++]="Hi15"; suffixes[si++]="Me15"; suffixes[si++]="Lo15";
    TString hisNames[9];
    hisNames[0] = "rawMcPiKP"; hisNames[1] = "accMcPiKP"; hisNames[2] = "matchedPlusMergedMc";
    hisNames[3] = "matMcPiKP"; hisNames[4] = "matchedSpectrumRec"; hisNames[5] = "rawSpectrumNoElectronBg";
    hisNames[6] = "rawSpectrumNoDecayBg"; hisNames[7] = "rawSpectrumNoGhosts";  hisNames[8] = "rawSpectrum";
    
    for (int i=0; i<9; ++i) {
	for (int j=0; j<18; ++j) {
	    TString curName = hisNames[i]+suffixes[j];
	    rawHistos[i][j] = (TH2D*) gROOT->FindObject(curName.Data());
	    //cout << (rawHistos[i][j])->GetName() << " " << (rawHistos[i][j])->GetEntries() << endl;
	}
    }
    int nbinsx  = rawHistos[0][0]->GetXaxis()->GetNbins();
    double lowx = rawHistos[0][0]->GetXaxis()->GetBinLowEdge(1);
    double hix  = rawHistos[0][0]->GetXaxis()->GetBinUpEdge(nbinsx);
    int nbinsy  = rawHistos[0][0]->GetYaxis()->GetNbins();
    double lowy = rawHistos[0][0]->GetYaxis()->GetBinLowEdge(1);
    double hiy  = rawHistos[0][0]->GetYaxis()->GetBinUpEdge(nbinsy);

    TDirectory* originalDir = gDirectory;
    TString dirname = originalDir->GetName();
    TString outfile = "CorHistosEvalItTest";
//     if (dirname.Contains("Embed")) {
// 	outfile += "Embed";
//     }
//     if (dirname.Contains("Hijing")) {
// 	outfile += "Hijing";
//     }
//     if (dirname.Contains("RevFullField")) {
// 	outfile += "RevFullField";
//     }
//     else {
// 	outfile += "FullField";
//     }
    outfile.Prepend(topDir);
    outfile += ".root";

    cout << outfile << endl;

    TString corNames[8];
    corNames[0] = "correctiongho"; corNames[1] = "correctionspl"; corNames[2] = "correctionebg"; corNames[3] = "correctiondbg";
    corNames[4] = "correctionres"; corNames[5] = "correctionmer"; corNames[6] = "correctioneff"; corNames[7] = "correctionacc";
    TH2D* correctiongho;
    TH2D* correctionspl;
    TH2D* correctionebg;
    TH2D* correctiondbg;
    TH2D* correctionres;
    TH2D* correctionmer;
    TH2D* correctioneff;
    TH2D* correctionacc;
    
    for (int cb=0; cb<18; ++cb) { // centrality bin, particle id loop
	TString curNameGho = corNames[0]+suffixes[cb];
	TString curNameSpl = corNames[1]+suffixes[cb];
	TString curNameEbg = corNames[2]+suffixes[cb];
	TString curNameDbg = corNames[3]+suffixes[cb];
	TString curNameRes = corNames[4]+suffixes[cb];
	TString curNameMer = corNames[5]+suffixes[cb];
	TString curNameEff = corNames[6]+suffixes[cb];
	TString curNameAcc = corNames[7]+suffixes[cb];
	
	correctiongho = new TH2D(curNameGho.Data(),"Ghost track correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctiongho->Sumw2();

	correctionspl = new TH2D(curNameSpl.Data(),"Split track correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctionspl->Sumw2();

	correctionebg = new TH2D(curNameEbg.Data(),"Electron Background correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctionebg->Sumw2();

	correctiondbg = new TH2D(curNameDbg.Data(),"Decay/Sec background correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctiondbg->Sumw2();

	correctionres = new TH2D(curNameRes.Data(),"Resolution correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctionres->Sumw2();

	correctionmer = new TH2D(curNameMer.Data(),"Merging correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctionmer->Sumw2();

	correctioneff = new TH2D(curNameEff.Data(),"Efficiency correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctioneff->Sumw2();

	correctionacc = new TH2D(curNameAcc.Data(),"Acceptance correction Function", nbinsx,lowx,hix,nbinsy,lowy,hiy);
	correctionacc->Sumw2();
	

	correctiongho->Divide(rawHistos[8][cb],rawHistos[7][cb],1.,1,"b"); // 
	correctiondbg->Divide(rawHistos[7][cb],rawHistos[6][cb],1.,1,"b"); // k0s, lambdas correction
	correctionebg->Divide(rawHistos[6][cb],rawHistos[5][cb],1.,1,"b"); // electrons
	correctionspl->Divide(rawHistos[5][cb],rawHistos[4][cb],1.,1,"b"); // split 
	correctionres->Divide(rawHistos[4][cb],rawHistos[3][cb],1.,1,"b"); // resolution
	correctionmer->Divide(rawHistos[3][cb],rawHistos[2][cb],1.,1,"b"); // merging
	correctioneff->Divide(rawHistos[2][cb],rawHistos[1][cb],1.,1,"b"); // efficiency
	correctionacc->Divide(rawHistos[1][cb],rawHistos[0][cb],1.,1,"b"); // includes acceptance + decay correction
//     divideWithBinomialErrors(correctiongho,rawSpectrum,rawSpectrumNoGhosts);
//     divideWithBinomialErrors(correctiondbg,rawSpectrumNoGhosts,rawSpectrumNoDecayBg); // k0s correction
//     divideWithBinomialErrors(correctionebg,rawSpectrumNoDecayBg,rawSpectrumNoElectronBg); // electrons
//     divideWithBinomialErrors(correctionspl,rawSpectrumNoElectronBg,matchedSpectrumRec);     //split                                              
//     divideWithBinomialErrors(correctionres,matchedSpectrumRec,matMcPiKP);                                                                
//     divideWithBinomialErrors(correctionmer,matMcPiKP,matchedPlusMergedMc);                                                            
//     divideWithBinomialErrors(correctioneff,matchedPlusMergedMc,accMcPiKP);                                                
//     divideWithBinomialErrors(correctionacc,accMcPiKP,rawMcPiKP); // includes acceptance + decay correction 
    
    //    return;
	TFile* effFile = new TFile(outfile,"UPDATE");
	if (!fromEmbedding) {
	    correctiongho->Write();
	    correctiondbg->Write();
	    correctionebg->Write();
	    correctionspl->Write();
	    correctionmer->Write();
	    cout << "Writing " << correctiondbg->GetName() << " " << correctiondbg->GetCellContent(11,5) << endl;
	}
	correctionres->Write();
	correctioneff->Write();
	correctionacc->Write();
	effFile->Close();
	cout << "Writing " << correctioneff->GetName() << " " << correctioneff->GetCellContent(11,5) << endl;
    }
    return;
    TCanvas* corrcnv = new TCanvas("corrcnv","Correction Functions",600,600*24/20);
    TCanvas* canvas = corrcnv;
    gPad->SetGridx();
    gPad->SetGridy();
    TPostScript* psfile;
    bool doPostScript = false;
    bool fromSimulation = true;
    if (doPostScript)
	psfile = new TPostScript("EmbeddingHighSet1Lim2.ps");

    correctiongho->ProjectionY("corrGhoPtAtEtaZero",11,11,"e");
    correctiongho->ProjectionY("corrGhoPtAllEta",6,15,"e");
//     errorsFrom2d(correctiondbg,corrDbgPtAtEtaZero);
    corrGhoPtAtEtaZero->SetMaximum(1.02);
    corrGhoPtAtEtaZero->SetMinimum(0.98);
    corrGhoPtAllEta->Scale(1./10.);
    corrGhoPtAllEta->SetMaximum(1.02);
    corrGhoPtAllEta->SetMinimum(0.98);
    
    histStyle(corrGhoPtAtEtaZero);
    histStyle(corrGhoPtAllEta);
    if (doPostScript && fromSimulation) { 
	psfile->NewPage();
    }
    corrGhoPtAllEta->Draw();
    
    canvas->Modified();
    canvas->Update();
//     }
    
    
    correctiondbg->ProjectionY("corrDbgPtAtEtaZero",11,11,"e");
    correctiondbg->ProjectionY("corrDbgPtAllEta",6,15,"e");
//     errorsFrom2d(correctiondbg,corrDbgPtAtEtaZero);
    corrDbgPtAtEtaZero->SetMaximum(1.2);
    corrDbgPtAtEtaZero->SetMinimum(0.9);
    corrDbgPtAllEta->Scale(1./10.);
    corrDbgPtAllEta->SetMaximum(1.2);
    corrDbgPtAllEta->SetMinimum(0.9);
    histStyle(corrDbgPtAtEtaZero);
    histStyle(corrDbgPtAllEta);
    if (doPostScript && fromSimulation) {
	psfile->NewPage();
    }
    corrDbgPtAllEta->Draw();
	
    canvas->Modified();
    canvas->Update();
//     }    

    correctionebg->ProjectionY("corrEbgPtAtEtaZero",11,11,"e");
    correctionebg->ProjectionY("corrEbgPtAllEta",6,15,"e");
//     errorsFrom2d(correctionebg,corrEbgPtAtEtaZero);
    corrEbgPtAtEtaZero->SetMaximum(2);
    corrEbgPtAtEtaZero->SetMinimum(0.8);
    corrEbgPtAllEta->Scale(1./10.);
    corrEbgPtAllEta->SetMaximum(2);
    corrEbgPtAllEta->SetMinimum(0.8);
    histStyle(corrEbgPtAtEtaZero);
    histStyle(corrEbgPtAllEta);
    if (doPostScript && fromSimulation) {
	psfile->NewPage();
    }
    corrEbgPtAllEta->Draw();
    
    canvas->Modified();
    canvas->Update();
//     }    

    correctionspl->ProjectionY("corrSplPtAtEtaZero",11,11,"e");
    correctionspl->ProjectionY("corrSplPtAllEta",6,15,"e");
//     errorsFrom2d(correctionspl,corrSplPtAtEtaZero);
    corrSplPtAtEtaZero->SetMaximum(1.04);
    corrSplPtAtEtaZero->SetMinimum(0.99);
    corrSplPtAllEta->Scale(1./10.);
    corrSplPtAllEta->SetMaximum(1.04);
    corrSplPtAllEta->SetMinimum(0.99);
    histStyle(corrSplPtAtEtaZero);
    histStyle(corrSplPtAllEta);

    if (doPostScript) {
	psfile->NewPage();
    }
    corrSplPtAllEta->Draw();
    
    canvas->Modified();
    canvas->Update();
//     }
    
    
    
    correctionres->ProjectionY("corrResPtAtEtaZero",11,11,"e");
    correctionres->ProjectionY("corrResPtAllEta",6,15,"e");
//     errorsFrom2d(correctionres,corrResPtAtEtaZero);
    corrResPtAtEtaZero->SetMaximum(1.6);
    corrResPtAtEtaZero->SetMinimum(0.2);
    corrResPtAllEta->Scale(1./10.);
    corrResPtAllEta->SetMaximum(1.6);
    corrResPtAllEta->SetMinimum(0.2);
    histStyle(corrResPtAtEtaZero);
    histStyle(corrResPtAllEta);

    if (doPostScript) {
	psfile->NewPage();
    }
    corrResPtAllEta->Draw();				  
    
    canvas->Modified();
    canvas->Update();
//     } 

    							  
    correctionmer->ProjectionY("corrMerPtAtEtaZero",11,11,"e");
    correctionmer->ProjectionY("corrMerPtAllEta",6,15,"e");
//     errorsFrom2d(correctionmer,corrMerPtAtEtaZero);
    corrMerPtAtEtaZero->SetMaximum(1.2);		  
    corrMerPtAtEtaZero->SetMinimum(.7);		  
    corrMerPtAllEta->Scale(1./10.);
    corrMerPtAllEta->SetMaximum(1.2);
    corrMerPtAllEta->SetMinimum(0.7);
    histStyle(corrMerPtAtEtaZero);
    histStyle(corrMerPtAllEta);
    if (doPostScript && fromSimulation) { 
	psfile->NewPage();
    }
    corrMerPtAllEta->Draw();				  
    
    canvas->Modified();
    canvas->Update();
//     }
    							  
    correctioneff->ProjectionY("corrEffPtAtEtaZero",11,11,"e");
    correctioneff->ProjectionY("corrEffPtAllEta",6,15,"e");
//     errorsFrom2d(correctioneff,corrEffPtAtEtaZero);
    corrEffPtAtEtaZero->SetMaximum(1.05);		  
    corrEffPtAtEtaZero->SetMinimum(.2);		  
    corrEffPtAllEta->Scale(1./10.);
    corrEffPtAllEta->SetMaximum(1.05);
    corrEffPtAllEta->SetMinimum(.2);		  
    histStyle(corrEffPtAtEtaZero);
    histStyle(corrEffPtAllEta);
    if (doPostScript) {
	psfile->NewPage();
    }
    corrEffPtAllEta->Draw();				  
    
    canvas->Modified();
    canvas->Update();
//     }   

    							  
    correctionacc->ProjectionY("corrAccPtAtEtaZero",11,11,"e");
    correctionacc->ProjectionY("corrAccPtAllEta",6,15,"e");
//     errorsFrom2d(correctionacc,corrAccPtAtEtaZero);
    corrAccPtAtEtaZero->SetMaximum(1.05);		  
    corrAccPtAtEtaZero->SetMinimum(0.2);		  
    corrAccPtAllEta->Scale(1./10.);
    corrAccPtAllEta->SetMaximum(1.05);
    corrAccPtAllEta->SetMinimum(0.2);
    histStyle(corrAccPtAtEtaZero);
    histStyle(corrAccPtAllEta);
    if (doPostScript) {
	psfile->NewPage();
    }
    corrAccPtAllEta->Draw();				  
    
    canvas->Modified();
    canvas->Update();
//     }   

    if (doPostScript) {
	psfile->Close();
	delete psfile;
    }
//    .! mv corrcnv.ps ~/private/plots/correctionFunctions/efficiencyEmbeddingCentral.ps
    
//     correctioneff->ProjectionY("corrPtAtEtaZero",11,11);
//     corrPtAtEtaZero->SetLineWidth(3);
//     corrPtAtEtaZero->SetLineColor(linecolor);

    
    
}

void checklevels() {
    correctioneff->ProjectionY("correffPtAtEtaZero",10,11,"e");
    correffPtAtEtaZero->Scale(.5);
    correffPtAtEtaZero->Fit("pol0","","",.4,2);
    cout << correffPtAtEtaZero->GetBinContent(2) << endl;
}

void makeptslices() {

    gStyle->SetPalette(1,0);
    
    correctiongho->ProjectionY("corrGhoPtAtEtaZero",11,11);
    correctionspl->ProjectionY("corrSplPtAtEtaZero",11,11);
    correctiondbg->ProjectionY("corrDbgPtAtEtaZero",11,11);
    correctionebg->ProjectionY("corrEbgPtAtEtaZero",11,11);
    correctionres->ProjectionY("corrResPtAtEtaZero",11,11);
    correctionmer->ProjectionY("corrMerPtAtEtaZero",11,11);
    correctioneff->ProjectionY("corrEffPtAtEtaZero",11,11);
    correctionacc->ProjectionY("corrAccPtAtEtaZero",11,11);
    
}
