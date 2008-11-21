#include "TDataProcessorMemberInspector.h"

#include <fstream>
#include <sstream>
using namespace std;

#include <TFolder.h>
#include <TClass.h>
#include <TDataMember.h>
#include <TROOT.h>
#include <TClonesArray.h>
#include <TSystem.h>
#include <TObjString.h>
#include <TCanvas.h>
#include <TBrowser.h>

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Analysis/TDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TInvariantMassDistribution.h>
#include <StEmcPool/StPi0Analysis/TBinStatistics.h>
#include <StEmcPool/StPi0Analysis/TDataProcessorPool.h>

ClassImp(TDataProcessorMemberInspector);
ClassImp(TMySignalHandler);

TMySignalHandler *mySignalHandler = 0;

TDataProcessorMemberInspector::TDataProcessorMemberInspector()
	: inherited() {
	this->debug = 0;
	this->mFolder = 0;
}

TDataProcessorMemberInspector::~TDataProcessorMemberInspector() {
}

void TDataProcessorMemberInspector::Inspect(TClass *cl, const char *parent, const char *name, const void *addr) {
	if (this->debug) cout << "TDataProcessorMemberInspector::Inspect(" << cl << ", " << parent << ", " << name << ", " << addr << ")" << endl;
	if (this->mFolder && cl && addr && (!parent || !strchr(parent,'.'))) {
		TDataMember *dataMember = cl->GetDataMember(name);
		if (dataMember) {
			TClass *dataClass = gROOT ? gROOT->GetClass(dataMember->GetTypeName()) : 0;
			if (dataClass) {
				if (dataClass->InheritsFrom("TH1")) {
					TH1 *hist = (TH1*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
					if (this->debug) cout << dataMember->GetTypeName() << " " << name << " = " << hist << ": InheritsFrom(TH1) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
					this->mFolder->Add(hist);
					if (this->debug) cout << "Added to folder" << endl;
				} else if (dataClass->InheritsFrom("TF1")) {
					if (this->debug) cout << dataMember->GetTypeName() << ": InheritsFrom(TF1) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
					this->mFolder->Add((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
					if (this->debug) cout << "Added to folder" << endl;
				} else {
					if (dataClass->InheritsFrom("TObject")) {
						if (this->debug) cout << dataMember->GetTypeName() << ": InheritsFrom(TNamed) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
						named_type *processor = dynamic_cast<named_type*>((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TFolder *folder = this->mFolder;
						if (this->debug) cout << "going to inspect " << addr << " " << processor << " <" << processor->GetName() << "> <" << processor->GetTitle() << ">" << endl;
						TString membername;// = processor->GetName();
						if (membername == "") {
						    membername = name;
						}
						TString membertitle;// = processor->GetTitle();
						if (membertitle == "") {
						    membertitle = dataMember->GetTypeName();;
						}
						TFolder *newFolder = this->Inspect(processor, this->mFolder, membername, membertitle);
						this->mFolder = folder;
						if (newFolder) {TCollection *list = newFolder->GetListOfFolders(); if (!list || list->IsEmpty()) {this->mFolder->Remove(newFolder); delete newFolder;}}
						if (this->debug) cout << "Added to folder" << endl;
					} else if (dataClass->InheritsFrom("list<TInvariantMassDistribution>")) {
						list<TInvariantMassDistribution> *listInv = (list<TInvariantMassDistribution>*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TFolder *newFolder = this->mFolder->AddFolder(name, "Distributions");
						if (newFolder) {
							for (list<TInvariantMassDistribution>::iterator iter = listInv->begin();iter != listInv->end();iter++) {
								TInvariantMassDistribution &inv = *iter;
								newFolder->Add(inv.getDistribution());
							}
						}
						if (newFolder) {TCollection *list = newFolder->GetListOfFolders(); if (!list || list->IsEmpty()) {this->mFolder->Remove(newFolder); delete newFolder;}}
					} else if (dataClass->InheritsFrom("list<TBinStatistics>")) {
						list<TBinStatistics> *listBin = (list<TBinStatistics>*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TFolder *newFolder = this->mFolder->AddFolder(name, "Bins");
						if (newFolder) {
							for (list<TBinStatistics>::iterator iter = listBin->begin();iter != listBin->end();iter++) {
								TBinStatistics &bin = *iter;
								TString nameStr = bin.GetName();
								if (nameStr == "") {
								    TBinParameters &params = bin.getParameters();
								    nameStr += "\"" + TString(Form("%g", params.min)) + TString((params.variable == pT) ? " < pT < " : ((params.variable == eGamma) ? " < E < " : " - ")) + TString(Form("%g", params.max)) + "\"";
								}
								TString titleStr = bin.GetTitle();
								nameStr += " = "; nameStr += Form("%g", bin.getValue());
								nameStr += " +/- "; nameStr += Form("%g", bin.getError());
								if (bin.getHistogram()) {
								    nameStr += " ("; nameStr += Form("%g", bin.getHistogram()->GetEntries()); nameStr += " entries)";
								}
								TNamed *named = new TNamed(nameStr.Data(), titleStr.Data());
								newFolder->Add(named);
							}
						}
						if (newFolder) {TCollection *list = newFolder->GetListOfFolders(); if (!list || list->IsEmpty()) {this->mFolder->Remove(newFolder); delete newFolder;}}
					} else if (strcmp(dataMember->GetTypeName(), "map<int,int,less<int>,allocator<pair<const int,int> > >") == 0) {
						map<int,int,less<int>,allocator<pair<const int,int> > > *cutsMap = (map<int,int,less<int>,allocator<pair<const int,int> > >*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TFolder *newFolder = this->mFolder->AddFolder(name, "Cut stats");
						if (newFolder) {
							for (map<int,int,less<int>,allocator<pair<const int,int> > >::const_iterator iter = cutsMap->begin();iter != cutsMap->end();iter++) {
								TString nameStr;
								const Char_t *cut_class_ = strstr(name, "_");
								TString cut_class_str(name, cut_class_ ? (cut_class_ - name) : 0);
								stringstream strstr;
								printCutNames((*iter).first, cut_class_str.Data(), strstr, " ");
								nameStr += strstr.str();
								nameStr += " = "; nameStr += (*iter).second;
								TString titleStr;
								TNamed *named = new TNamed(nameStr.Data(), titleStr.Data());
								newFolder->Add(named);
							}
						}
						if (newFolder) {TCollection *list = newFolder->GetListOfFolders(); if (!list || list->IsEmpty()) {this->mFolder->Remove(newFolder); delete newFolder;}}
					} else if (dataClass->InheritsFrom("list<TDataProcessor*>")) {
						list<TDataProcessor*> *listProc = (list<TDataProcessor*>*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TFolder *folder = this->mFolder;
						for (list<TDataProcessor*>::iterator iter = listProc->begin();iter != listProc->end();iter++) {
							TDataProcessor *processor = *iter;
							this->Inspect(processor, folder);
						}
						this->mFolder = folder;
					} else if (dataClass->InheritsFrom("TString")) {
						TString *str = (TString*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TString nameStr = name; nameStr += " = """; nameStr += *str; nameStr += """";
						this->mFolder->Add(new TNamed(nameStr.Data(), ""));
					} else if (TString(dataMember->GetTypeName()) == "event_list_type") {
						list<pair<Int_t, Int_t> > *listEv = (list<pair<Int_t, Int_t> >*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
						TString nameStr = Form("%s: %i events", name, listEv->size());
						this->mFolder->Add(new TNamed(nameStr.Data(), ""));
					} else {
						cerr << "Don't know what to do with class " << dataMember->GetTypeName() << " " << name << " !!!" << endl;
					}
				}
			} else {
				if (((strcmp(dataMember->GetTypeName(), "UInt_t") == 0) || (strcmp(dataMember->GetTypeName(), "Int_t") == 0) || (strcmp(dataMember->GetTypeName(), "int") == 0))) {
					Bool_t addThis = true;
					Bool_t isArray = false;
					if (strcmp(name, "[") == 0) isArray = true;
					Int_t arrayIndex = 0;
					TString nameStr = name;
					nameStr += " =";
					Int_t num = 0;
					do {
					    if (strcmp(dataMember->GetTypeName(), "UInt_t") == 0) {
						num = (Int_t)(*((UInt_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)) + arrayIndex));
					    } else {
						num = *((Int_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)) + arrayIndex);
					    }
					    if (strstr(name, "_CUTS")) {
						if (num != 0) {
							const Char_t *cut_class_ = strstr(name, "_");
							TString cut_class_str(name, cut_class_ ? (cut_class_ - name) : 0);
							stringstream strstr;
							printCutNames(num, cut_class_str.Data(), strstr, ", ");
							nameStr += " ";
							nameStr += strstr.str();
						} else {nameStr += " (not set)"; /*addThis = false;*/}
					    } else if (strstr(name, "numPassed") || strstr(name, "numTotal")) {
						TString unit = "";
						Float_t number = num;
						Bool_t append = true;
						if (num > 1000000) {
						    number = Float_t(num) / 1000000.0;
						    unit = "M";
						} else if (num > 1000) {
						    number = Float_t(num) / 1000.0;
						    unit = "K";
						} else {
						    append = false;
						}
						if (append) {
    						    nameStr += " "; nameStr += Form("%.1f %s (%i)", number, unit.Data(), num);
						} else {
    						    nameStr += " "; nameStr += num;					    
						}
					    } else {
						nameStr += " "; nameStr += num;
					    }
					    arrayIndex++;
					} while (isArray && (num != 0) && (num != -1));
					TNamed *named = addThis ? (new TNamed(nameStr.Data(), "")) : 0;
					if (named) this->mFolder->Add(named);
				} else if (strcmp(dataMember->GetTypeName(), "Float_t") == 0) {
					Float_t num = *((Float_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)));
					TString nameStr = name; nameStr += " = "; nameStr += Form("%g", num);
					TNamed *named = new TNamed(nameStr.Data(), "");
					this->mFolder->Add(named);
				} else if (strcmp(dataMember->GetTypeName(), "ULong_t") == 0) {
					ULong_t num = *((ULong_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)));
					TString nameStr = name; nameStr += " = "; nameStr += Form("%i", num);
					TNamed *named = new TNamed(nameStr.Data(), "");
					this->mFolder->Add(named);
				} else if (strcmp(dataMember->GetTypeName(), "Double_t") == 0) {
					Double_t num = *((Double_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)));
					TString nameStr = name; nameStr += " = "; nameStr += Form("%g", num);
					TNamed *named = new TNamed(nameStr.Data(), "");
					this->mFolder->Add(named);
				} else if (strcmp(dataMember->GetTypeName(), "Bool_t") == 0) {
					Bool_t num = *((Bool_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)));
					TString nameStr = name; nameStr += " = "; nameStr += num ? "true" : "false";
					TNamed *named = new TNamed(nameStr.Data(), "");
					this->mFolder->Add(named);
				} else if ((strcmp(dataMember->GetTypeName(), "Char_t") == 0) || (strcmp(dataMember->GetTypeName(), "char") == 0) || (strcmp(dataMember->GetTypeName(), "Option_t") == 0)) {
					TString str;
					if (*name == '*') {
					    Char_t *cstr = (Char_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
					    str = cstr;
					} else {
					    Char_t *cstr = (Char_t*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr));
					    str = cstr[0];
					}
					TString nameStr = (*name == '*') ? (name+1) : (name); nameStr += " = """; nameStr += str; nameStr += """";
					this->mFolder->Add(new TNamed(nameStr.Data(), ""));
				} else if (strcmp(dataMember->GetTypeName(), "TBinVariable") == 0) {
					TBinVariable num = *((TBinVariable*)((*name == '*') ? (*((TObject**)addr)) : ((TObject*)addr)));
					TString nameStr = name; nameStr += " = "; nameStr += ((num == pT) ? "pT" : ((num == eGamma) ? "eGamma" : Form("%g", num)));
					TNamed *named = new TNamed(nameStr.Data(), "");
					this->mFolder->Add(named);
				} else {
				    cerr << "Don't know what to do with " << dataMember->GetTypeName() << " " << name << " !!!" << endl;
				}
			}
		}
	}
	if (this->debug) cout << "TDataProcessorMemberInspector::Inspect() finished" << endl;
}

TDataProcessorMemberInspector::folder_type *TDataProcessorMemberInspector::Inspect(named_type &named, folder_type *folder, const TString &name, const TString &title) {
	if (this->debug) cout << "Inspect " << &named << " <" << name << "> <" << title << ">" << endl;
	this->mFolder = (folder) ? folder->AddFolder(name, title) : 0;
	Char_t buffer[1024]; buffer[0] = 0;
	if (this->debug) cout << "go ShowMembers..." << endl;
	named.ShowMembers(*this, buffer);
	if (this->debug) cout << "Inspect finished." << endl;
	return this->mFolder;
}

TMySignalHandler::TMySignalHandler(ESignals sig, TString fileIn, TString fileOut, Bool_t sync)
    : TSignalHandler(sig, sync)
    , filenameIn(fileIn)
    , filenameOut(fileOut)
    , timer(0)
{
}

void listFolder(const TFolder *folder, const TString &namePrefix, TString &listing) {
    if (folder) {
//cout << "list folder " << namePrefix << " / " << folder->GetName() << endl;
	TCollection *children = folder->GetListOfFolders();
	if (children) {
	    TIter iter(children);
	    TNamed *obj = 0;
	    while ((obj = (TNamed*)iter.Next())) {
		if (obj) {
//cout << "child " << namePrefix << " / " << folder->GetName() << " / " << obj->GetName() << endl;
		    const TFolder *childFolder = dynamic_cast<const TFolder*>(obj);
		    const TH1 *childHist = dynamic_cast<const TH1*>(obj);
		    const TH2 *childHist2 = childHist ? dynamic_cast<const TH2*>(obj) : 0;
		    TString x = childHist2 ? 
			(childFolder ? (childHist ? "dr-xr-xr-x 001     root     root 00000002 01-01-2020 01:01 " : "dr--r--r-- 001     root     root 00000002 01-01-2020 01:01 ") : (childHist ? "-r-xr-xr-x 001     root     root 00000002 01-01-2020 01:01 " : "-r--r--r-- 001     root     root 00000002 01-01-2020 01:01 "))
			:
			(childFolder ? (childHist ? "dr-xr-xr-x 001     root     root 00000001 01-01-2020 01:01 " : "dr--r--r-- 001     root     root 00000001 01-01-2020 01:01 ") : (childHist ? "-r-xr-xr-x 001     root     root 00000001 01-01-2020 01:01 " : "-r--r--r-- 001     root     root 00000001 01-01-2020 01:01 "))
			;
		    TString line = x + namePrefix + obj->GetName() + "\n";
		    listing += line;
		    if (childFolder) {
			TString listingChild;
			listFolder(childFolder, namePrefix + childFolder->GetName() + "/", listingChild);
			listing += listingChild;
		    }
		}
	    }
	} else cout << "no children!" << endl;
    } else cout << "no folder!" << endl;
}

TFolder *getFileFolder(TString poolFile) {
	TFolder *poolFolder = 0;
        TFolder *dataFolder = 0;
        TFolder *rootFolder = gROOT->GetRootFolder();
	if (rootFolder) {
	    dataFolder = (TFolder*)rootFolder->FindObject("processedData");
	    if (!dataFolder) {
		dataFolder = rootFolder->AddFolder("processedData", "Processed data");
	    } else cout << "data folder already exists" << endl;
	} else cout << "No root folder!" << endl;
	if (dataFolder) {
	    TString poolFolderName = poolFile;
	    poolFolderName.ReplaceAll("/","~");
	    poolFolder = (TFolder*)dataFolder->FindObject(poolFolderName);
	    if (!poolFolder) {
		TDataProcessorPool *pool = new TDataProcessorPool(poolFolderName, poolFile);
		if (pool) {
		    cout << "start processing pool file..." << endl;
		    pool->processFile(poolFile);
    		    TDataProcessorMemberInspector memberInspector;
		    cout << "start inspecting pool..." << endl;
		    memberInspector.Inspect(pool, dataFolder);
		    cout << "finished inspecting pool" << endl;
		} else cout << "Cannot create pool!" << endl;
	    } else cout << "pool folder already exists" << endl;
	    poolFolder = (TFolder*)dataFolder->FindObject(poolFolderName);
	}
	return poolFolder;
}

Bool_t TMySignalHandler::Notify() {
    
    if (this->timer) this->timer->Stop();
    if (this->timer) this->timer->Start(-1, false);

    if (this->GetSignal() == kSigTermination) {
	cout << "Termination signal..." << endl;
	if (this->filenameIn != "") {
	    cout << "deleting " << this->filenameIn << endl;
	    gSystem->Exec(TString("rm -f " + this->filenameIn).Data());
	}
	if (this->filenameOut != "") {
	    cout << "deleting " << this->filenameOut << endl;
	    gSystem->Exec(TString("rm -f " + this->filenameOut).Data());
	}
	cout << "Exit(0)..." << endl;
	gSystem->Exit(0);
	return true;
    }
    cout << "TMySignalHandler: signal notify" << endl;

    TString arg0;
    TString arg1;
    TString arg2;
    TString arg3;
    TString arg4;
    cout << "reading file " << this->filenameIn.Data() << endl;
    ifstream ifstr(this->filenameIn.Data());
    if (ifstr.good()) {
	    TString str;
	    str.ReadLine(ifstr);
	    cout << "string read: " << str << endl;
            TObjArray *tokens = str.Tokenize(TString(" "));
            if (tokens) {
		if (tokens->GetEntries() > 0) {
            	    TObjString *str0 = dynamic_cast<TObjString*>(tokens->At(0));
            	    if (str0) arg0 = str0->GetString();
		}
		if (tokens->GetEntries() > 1) {
            	    TObjString *str1 = dynamic_cast<TObjString*>(tokens->At(1));
            	    if (str1) arg1 = str1->GetString();
		}
		if (tokens->GetEntries() > 2) {
            	    TObjString *str2 = dynamic_cast<TObjString*>(tokens->At(2));
            	    if (str2) arg2 = str2->GetString();
		}
		if (tokens->GetEntries() > 3) {
            	    TObjString *str3 = dynamic_cast<TObjString*>(tokens->At(3));
            	    if (str3) arg3 = str3->GetString();
		}
		if (tokens->GetEntries() > 4) {
            	    TObjString *str4 = dynamic_cast<TObjString*>(tokens->At(4));
            	    if (str4) arg4 = str4->GetString();
		}
		delete tokens;
	    }
    } else {
	cout << "cannot open file!" << endl;
    }
    cout << "arg0=" << arg0 << " arg1=" << arg1 << " arg2=" << arg2 << " arg3=" << arg3 << " arg4=" << arg4 << endl;
    
    if (arg0 == "list") {
	TString listing;
	TFolder *poolFolder = getFileFolder(arg1);
	if (poolFolder) {
        	new TBrowser(poolFolder->GetName(), poolFolder, poolFolder->GetTitle());
		cout << "Start listing..." << endl;
		listFolder(poolFolder, "", listing);
		cout << "Listing:" << endl << listing << endl;
	} else cout << "No pool folder!" << endl;
	cout << "writing file " << this->filenameOut.Data() << endl;
	ofstream ofstr(this->filenameOut.Data());
	if (ofstr.good()) {
	    ofstr << listing.Data();
	} else cout << "cannot write to file!" << endl;
    } else if (arg0 == "run") {
	TFolder *poolFolder = getFileFolder(arg1);
	if (poolFolder) {
		cout << "running " << arg2 << endl;
        	TObjArray *tokens = arg2.Tokenize(TString("/"));
        	if (tokens) {
		    TFolder *dataFolder = poolFolder;
		    TObject *object = dataFolder;
		    for (Int_t i = 0;i < tokens->GetEntries();i++) {
            		TObjString *str = dynamic_cast<TObjString*>(tokens->At(i));
            		if (str) {
			    TString name = str->GetString();
			    if (name != "") {
				cout << "looking for " << name << endl;
				object = dataFolder ? dataFolder->FindObject(name) : 0;
				if (object) {
				    dataFolder = dynamic_cast<TFolder*>(object);
				    if (dataFolder) cout << "this is a folder" << endl;
				} else cout << "no object!" << endl;
			    }
			}
		    }
		    delete tokens;
		    if (object) {
			TH1 *h1 = dynamic_cast<TH1*>(object);
			if (h1) {
			    cout << "this is a histogram, drawing..." << endl;
			    TCanvas *c = new TCanvas(h1->GetName(), h1->GetTitle());
			    if (c) c->cd();
			    h1->Draw("h colz");
			}
		    } else cout << "no object found!" << endl;
		}
	} else cout << "No pool folder!" << endl;
	cout << "writing file " << this->filenameOut.Data() << endl;
	ofstream ofstr(this->filenameOut.Data());
	if (ofstr.good()) {
	    ofstr << "run" << endl;
	} else cout << "cannot write to file!" << endl;
    }

    if (this->timer) this->timer->Stop();
    if (this->timer) this->timer->Start(-1, false);

    cout << "TMySignalHandler: signal exit" << endl;
    return true;
}
