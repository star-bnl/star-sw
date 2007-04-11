#include "TFile.h"
//#include "TCanvas.h"
//#include "TH1.h"
//#include <smdGain.h>
//#include <St_smdGain_Table.h>

void transformBackupHistoToDBTable(Char_t *hist_filename = "./backup.emconline_ped/bemcPed.20060302.084014.root", Char_t *tables_path = "./tables.emconline_ped/") {
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StDbLib");
    gSystem->Load("StEmcUtil");
    
    TString inputFilename(hist_filename);
    cout << "Input filename is " << inputFilename.Data() << endl;
    
    TString outputFilename;
    TString tableName;
    
    TObjArray *dirNames = inputFilename.Tokenize(TString("/"));

    if (dirNames) {
	cout << "Input filename tokenized" << endl;
	TObjString *lastFileName = (TObjString*)dirNames->Last();
	if (lastFileName) {
	    outputFilename = lastFileName->GetString();
	    cout << "File is " << outputFilename.Data() << endl;
	    TObjArray *pieces = outputFilename.Tokenize(".");
	    if (pieces) {
		cout << "File name tokenized" << endl;
		TObjString *firstPiece = (TObjString*)pieces->First();
		if (firstPiece) {
		    tableName = firstPiece->GetString();
		    cout << "Table name is " << tableName.Data() << endl;
		}
		delete pieces;
	    }
	}
	delete dirNames;
    }

    if (outputFilename.Length() != 0) outputFilename.Prepend("/");

    int detector = 0;
    if (tableName == "bemcPed") {
	detector = 1;
	if (outputFilename.Length() != 0) outputFilename.Prepend("/y3bemc");
    }
    if (tableName == "bsmdePed") {
	detector = 2;
	if (outputFilename.Length() != 0) outputFilename.Prepend("/y3bsmde");
    }
    if (tableName == "bsmdpPed") {
	detector = 3;
	if (outputFilename.Length() != 0) outputFilename.Prepend("/y3bsmdp");
    }
    if (tableName == "bprsPed") {
	detector = 4;
	if (outputFilename.Length() != 0) outputFilename.Prepend("/y3bprs");
    }

    if (outputFilename.Length() != 0) outputFilename.Prepend(tables_path);

    cout << "Output filename is " << outputFilename.Data() << endl;
    cout << "Output table name is " << tableName.Data() << endl;
    
    if (detector == 0) {
	cout << "Unknown detector!" << endl;
	return;
    }
    
    TFile *hist_file = new TFile(hist_filename, "READ");
    TH1F *ped = 0;
    TH1F *rms = 0;
    TH1F *chi = 0;
    TH1F *status = 0;
    if (hist_file && hist_file->IsOpen()) {
	cout << "Input file is open" << endl;
        ped = dynamic_cast<TH1F*>(hist_file->Get("mPed"));
	cout << "Read ped = " << ped << endl;
        rms = dynamic_cast<TH1F*>(hist_file->Get("mRms"));
	cout << "Read rms = " << rms << endl;
        chi = dynamic_cast<TH1F*>(hist_file->Get("mChi"));
	cout << "Read chi = " << chi << endl;
        status = dynamic_cast<TH1F*>(hist_file->Get("mStatus"));
	cout << "Read status = " << status << endl;
    }

    TFile *table_file = 0;
    if (ped && rms && chi && status) table_file = new TFile(outputFilename.Data(), "RECREATE");
    
    if (table_file && table_file->IsOpen()) {
	cout << "Output file open" << endl;

	if ((detector == 1) || (detector == 4)) {
	    St_emcPed *st = new St_emcPed(tableName.Data(), 1);
    	    emcPed_st *rec = st->GetTable();
	    for (Int_t i = 0;i < 4800;i++) {
		Float_t pedestal = (i < ped->GetXaxis()->GetNbins()) ? ped->GetBinContent(i + 1) : 0;
		Float_t pedestalRms = (i < rms->GetXaxis()->GetNbins()) ? rms->GetBinContent(i + 1) : 0;
		Float_t pedestalChi = (i < chi->GetXaxis()->GetNbins()) ? chi->GetBinContent(i + 1) : 0;
		Float_t pedestalStatus = (i < status->GetXaxis()->GetNbins()) ? status->GetBinContent(i + 1) : 1;
	    	cout << i << ": ped = " << pedestal << ", rms = " << pedestalRms << ", chi = " << pedestalChi << ", status = " << pedestalStatus << endl;
		rec->AdcPedestal[i] = pedestal * 100;
		rec->AdcPedestalRMS[i] = pedestalRms * 100;
		rec->Status[i] = pedestalStatus;
	    }
    	    st->AddAt(rec, 0);
	    st->Write();
	}
	
	if ((detector == 2) || (detector == 3)) {
	    St_smdPed *st = new St_smdPed(tableName.Data(), 1);
    	    smdPed_st *rec = st->GetTable();
	    for (Int_t i = 0;i < 18000;i++) {
		Float_t pedestal = (i < ped->GetXaxis()->GetNbins()) ? ped->GetBinContent(i + 1) : 0;
		Float_t pedestalRms = (i < rms->GetXaxis()->GetNbins()) ? rms->GetBinContent(i + 1) : 0;
		Float_t pedestalChi = (i < chi->GetXaxis()->GetNbins()) ? chi->GetBinContent(i + 1) : 0;
		Float_t pedestalStatus = (i < status->GetXaxis()->GetNbins()) ? status->GetBinContent(i + 1) : 1;
	    	cout << i << ": ped = " << pedestal << ", rms = " << pedestalRms << ", chi = " << pedestalChi << ", status = " << pedestalStatus << endl;
		rec->AdcPedestal[i][0] = pedestal * 100;
		rec->AdcPedestalRMS[i][0] = pedestalRms * 100;
		rec->Status[i] = pedestalStatus;
	    }
    	    st->AddAt(rec, 0);
	    st->Write();
	}
	
	table_file->Write();
	cout << "Output file written" << endl;
    }

    if (table_file) {
	table_file->Close();
	delete table_file;
	cout << "Output file closed" << endl;
    }
    
    if (hist_file) delete hist_file;
    cout << "Finished." << endl;
}
