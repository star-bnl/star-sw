#include "TDataProcessorPool.h"
#include "TDataProcessor.h"

#include <TTree.h>
#include <TFile.h>
#include <TH1.h>
#include <TH1F.h>
#include <TKey.h>
#include <TClass.h>
#include <TClonesArray.h>
#include <TSystem.h>
#include <TObjString.h>

//#include <iostream>
#include <fstream>
using namespace std;

#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

ClassImp(TDataProcessorPool);

pools_list_type poolsList;

void removePool(const TDataProcessorPool *pool, Bool_t deletePool, Bool_t deleteProcessors) {
    if (pool) {
	TDataProcessorPool *poolExisting = 0;
	for (pools_list_type::iterator iter = poolsList.begin();(iter != poolsList.end()) && (!poolExisting);++iter) {
	    if ((*iter) == pool) {
		poolExisting = *iter;
	    }
	}
	if (poolExisting) {
	    poolsList.remove(poolExisting);
	    if (deleteProcessors) {
		poolExisting->deleteProcessors();
	    }
    	    if (deletePool) {
		//cout << "Deleting pool " << poolExisting << endl;
		delete poolExisting;
	    }
	}
    }
}

TDataProcessorPool::TDataProcessorPool(const Char_t *name, const Char_t *title)
	: inherited(name, title) {
	poolsList.push_back(this);
	this->debug = 0;
	this->outputPrescale = 0;
	this->writeArraySize = 1000;
	this->maxVirtualSize = 25*1024*1024;
	this->splitLevel = 99;
	this->basketSize = 32000;
	this->compression = 9;
}

TDataProcessorPool::TDataProcessorPool(const this_type &pool)
	: inherited() {
	poolsList.push_back(this);
	this->debug = 0;
	this->outputPrescale = 0;
	this->writeArraySize = 1000;
	this->maxVirtualSize = 25*1024*1024;
	this->basketSize = 32000;
	this->splitLevel = 99;
	this->compression = 9;
	this->operator=(pool);
}

TDataProcessorPool::~TDataProcessorPool() {
	removePool(this, false, false);
}

TDataProcessorPool::this_type &TDataProcessorPool::operator=(const this_type &pool) {
	this->inherited::operator=(pool);

	this->processors = pool.processors;
	this->debug = pool.debug;
	this->outputPrescale = pool.outputPrescale;

	return *this;
}

TDataProcessorPool::processor_type_pointer TDataProcessorPool::getProcessor(const Char_t *name) {
	processor_type_pointer processor = 0;
	for (list_type::iterator iter = this->processors.begin();iter != this->processors.end();iter++) {
		processor_type_pointer &proc = *iter;
		if (proc) {
			if (strcmp(name, proc->GetName()) == 0) processor = proc;
		}
	}
	return processor;
}

void TDataProcessorPool::deleteProcessors() {
	for (list_type::iterator iter = this->processors.begin();iter != this->processors.end();iter++) {
		processor_type_pointer &proc = *iter;
		if (proc) {
		    //cout << "Deleting processor " << proc << " " << proc->GetName() << endl;
		    delete proc;
		}
	}
	this->processors.clear();
}

TDataProcessorPool::const_processor_type_pointer TDataProcessorPool::getProcessor(const Char_t *name) const {
	const_processor_type_pointer processor = 0;
	for (list_type::const_iterator iter = this->processors.begin();iter != this->processors.end();iter++) {
		const processor_type_pointer &proc = *iter;
		if (proc) {
			if (strcmp(name, proc->GetName()) == 0) processor = proc;
		}
	}
	return processor;
}

void TDataProcessorPool::Print(Option_t* option) const {
	const Char_t *prefix = option ? ((const Char_t *)option) : "";
	const Char_t *tab = "\t";
	cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
	cout << prefix << tab << "debug = " << this->debug << endl;
	cout << prefix << tab << "writeArraySize = " << this->writeArraySize << endl;
	cout << prefix << tab << "maxVirtualSize = " << this->maxVirtualSize << endl;
	cout << prefix << tab << "basketSize = " << this->basketSize << endl;
	cout << prefix << tab << "splitLevel = " << this->splitLevel << endl;
	cout << prefix << tab << "compression = " << this->compression << endl;
	TString newPrefix(prefix);
	newPrefix += tab;
	for (list_type::const_iterator iter = this->processors.begin();iter != this->processors.end();++iter) {
		const processor_type *proc = *iter;
		if (proc) proc->Print(newPrefix.Data());
	}
}

Bool_t TDataProcessorPool::processFile(const TFile *inFile, TFile *outFile) {
	Bool_t result = true;
	if (this->debug) cout << "TDataProcessorPool::processFile started" << endl;
	if (inFile) {
		cout << "Processing file " << inFile->GetName() << endl;
		TIter inIter(inFile->GetListOfKeys());
		while(TKey *key = dynamic_cast<TKey*>(inIter())) {
			if (this->debug) cout << "Read key " << key->GetName() << endl;
			TObject *obj = key->ReadObj();
			if (!obj) continue;
			if (obj->InheritsFrom(TTree::Class_Name())) {
				if (this->debug) cout << "This is a tree " << obj->GetName() << " " << obj->GetTitle() << endl;
				TTree *readTree = dynamic_cast<TTree*>(obj);
				if (readTree) {
					Bool_t isPlainTree = false;
					TTree *writeTree = 0;
					TClonesArray *writeArray = 0;
					Int_t writeArrayIndex = 0;
					TBranch *branch = 0;
					TObjArray *branches = readTree->GetListOfBranches();
					if (branches) {
					    branch = dynamic_cast<TBranch*>(branches->At(0));
					    if (branch) {
						TString branchName = branch->GetClassName();
						isPlainTree = (branchName != TClonesArray::Class_Name());
						if (this->debug) cout << "isPlainTree = " << isPlainTree << endl;
					    }
					    if (branch && outFile) {
						if (isPlainTree) {
						    writeArray = new TClonesArray(getBranchType(branch->GetName()), 0);
						    if (writeArray) {
							writeArray->ExpandCreate(this->writeArraySize);
							writeArray->BypassStreamer(true);
					    	        writeTree = createTree(outFile, readTree->GetName(), readTree->GetTitle(), branch->GetName(), writeArray->Class_Name(), writeArray, this->maxVirtualSize, this->basketSize, this->splitLevel);
						    }
						} else {
					    	    writeTree = createTree(outFile, readTree->GetName(), readTree->GetTitle(), branch->GetName(), branch->GetClassName(), 0, this->maxVirtualSize, this->basketSize, this->splitLevel);
						}
					    }
					}
					processor_type_pointer *procs = new processor_type_pointer[this->processors.size()];
					if (procs) {
					    Int_t procsNum = 0;
					    Bool_t processorExist = false;
					    for (list_type::iterator iter = this->processors.begin();iter != this->processors.end();++iter) {
						processor_type_pointer &processor = *iter;
						if (processor && processor->getTreeName() && (strcmp(processor->getTreeName(), readTree->GetName()) == 0)) {
						    procs[procsNum++] = processor;
						    processorExist = true;
						    cout << processor->GetTitle() << ": reading " << readTree->GetEntries() << " entries from " << readTree->GetTitle() << endl;						    
						} else {
						    procs[procsNum++] = 0;
						}
					    }
					    Int_t nentries = Int_t(readTree->GetEntries());
					    for (Int_t entryInd = 0;entryInd < nentries;entryInd++) {
						TClonesArray *arr = 0;
						TObject *plainStructure = 0;
						if (debug) cout << "Getting entry " << entryInd << ": setting input branch address" << endl;
						if (isPlainTree) {
						    readTree->SetBranchAddress(branch->GetName(), &plainStructure);
						    if (debug) cout << "set plain structure address" << endl;
						} else {
						    readTree->SetBranchAddress(branch->GetName(), &arr);
						    if (debug) cout << "set array address" << endl;
						}
						if (debug) cout << "Done" << endl;
						if (readTree->GetEntry(entryInd) <= 0) {
						    cout << "Cannot get entry " << entryInd << endl;
						} else {
						    if (debug) cout << "Got entry " << entryInd << ": arr=" << arr << ", plain=" << plainStructure << endl;
						    if ((entryInd % outputPrescale) == (outputPrescale / 2)) cout << "Processing entry " << entryInd << " of " << nentries << " (" << Float_t(Int_t(entryInd * 1000.0 / nentries))  / 10.0 << "%)..." << endl;
						    Int_t nentries2 = isPlainTree ? 1 : (arr ? arr->GetEntries() : 0);
						    if (debug) cout << "nentries2=" << nentries2 << endl;
						    for (Int_t j = 0;j < nentries2;j++) {
							if (debug) { cout << ","; cout.flush(); }
							TObject *obj = isPlainTree ? plainStructure : arr->AddrAt(j);
							if (debug) { cout << "'"; cout.flush(); }
							if (obj) {
							    bool accepted = false;
							    if (processorExist) {
								for (Int_t i = 0;i < (Int_t)this->processors.size();i++) {
								    processor_type_pointer processor = procs[i];
								    if (processor) {
									if (debug) { cout << "\""; cout.flush(); }
									accepted |= processor->process((void*)obj, (void*)0, -1.0);
									if (debug) { cout << "`"; cout.flush(); }
								    }
								}
							    } else {
								accepted = true;
							    }
							    if (accepted && writeTree) {
								if (!isPlainTree && writeArray) {
								    TObject *writeData = dynamic_cast<TObject*>(writeArray->AddrAt(writeArrayIndex));
								    if (writeData) {
									obj->Copy(*writeData);
							    		writeArrayIndex++;
									if (writeArrayIndex >= this->writeArraySize) {
									    writeTree->SetBranchAddress(branch->GetName(), &writeArray);
									    writeTree->Fill();
									    writeArrayIndex = 0;
									}
								    }
								} else if (isPlainTree && plainStructure) {
								    writeTree->SetBranchAddress(branch->GetName(), &plainStructure);
								    writeTree->Fill();
								}
							    }
							}
							if (debug) { cout << "."; cout.flush(); }
						    }	
						    if (arr) delete arr;
						    if (plainStructure) delete plainStructure;
    						}
					    }
					    delete [] procs;
					}
					if (writeArray) {
					    for (Int_t i = writeArrayIndex;i < this->writeArraySize;i++) writeArray->RemoveAt(i);
					    writeArray->Compress();
					}
					if (writeTree && writeArray && branch) {
					    writeTree->SetBranchAddress(branch->GetName(), &writeArray);
					    writeTree->Fill();
					}
					if (outFile) {
					    outFile->Write(0, TObject::kOverwrite);
					}
					if (writeArray) delete writeArray;
					if (writeTree) delete writeTree;
				}
			} else if (obj->InheritsFrom(TH1::Class_Name())) {
				if (this->debug) cout << "This is a histogram" << endl;
				TH1 *readHisto = dynamic_cast<TH1*>(obj);
				if (readHisto) {
					for (list_type::iterator iter = this->processors.begin();iter != this->processors.end();++iter) {
						processor_type *processor = *iter;
						if (processor && processor->getHistogramName() && (strcmp(processor->getHistogramName(), readHisto->GetName()) == 0)) {
							if (debug) cout << processor->GetName() << " wants to process it" << endl;
							processor->process((void*)readHisto, (void*)0, -1.0);
						}
					}
				    if (outFile) {
					outFile->WriteObject(readHisto, readHisto->GetName());
				    }
				}
			} else if (obj->InheritsFrom(TDataProcessor::Class_Name())) {
				if (this->debug) cout << "This is a data processor" << endl;
				processor_type *readProcessor = dynamic_cast<processor_type *>(obj);
				if (readProcessor) {
					Bool_t added = false;
					for (list_type::iterator iter = this->processors.begin();(iter != this->processors.end()) && (!added);++iter) {
						processor_type *processor = *iter;
						if (processor) {
							Bool_t thisAdded = processor->add(*readProcessor);
							added |= thisAdded;
							if (thisAdded && this->debug) cout << "Added to " << processor->GetName() << endl;
						}
					}
					if (!added) {
						if (this->debug) cout << "Not added, trying to create new" << endl;
						processor_type *newProcessor = dynamic_cast<processor_type*>(readProcessor->Clone(readProcessor->GetName()));
						if (newProcessor) {
							if (debug) cout << "New processor created" << endl;
							this->processors.push_back(newProcessor);
							if (debug) cout << "New processor added" << endl;
							added = true;
						} else {
							if (debug) cout << "Cannot create new processor" << endl;
						}
					}
					result &= added;
					if (outFile) {
					    outFile->WriteObject(readProcessor, readProcessor->GetName());
					}
				}
			} else if (obj->InheritsFrom(TMyDataAnalysisSettings::Class_Name())) {
				if (this->debug) cout << "This is TMyDataAnalysisSettings" << endl;
				TMyDataAnalysisSettings *settings = dynamic_cast<TMyDataAnalysisSettings *>(obj);
				if (settings) {
				    if (this->debug) cout << "Copying " << key->GetName() << endl;
				    this->settings = (*settings);
				    if (outFile) {
					if (this->debug) cout << "Writing settings out" << endl;
					outFile->WriteObject(&this->settings, key->GetName());
				    }
				}
			} else {
				if (this->debug) cout << "Don't know what to do with " << obj->Class_Name() << " " << obj->GetName() << endl;
			}
			delete obj;
		}
	}
	if (this->debug) cout << "TDataProcessorPool::processFile finished: " << result << endl;
	return result;
}

Bool_t TDataProcessorPool::processFile(const Char_t *filename, const Char_t *outPath) {
	if (this->debug) cout << "TDataProcessorPool::processFile started: " << (filename ? filename : "") << endl;
	Bool_t result = false;
	if (filename && (*filename)) {
		Bool_t isFileList = false;
		Bool_t isFile = false;
		const Char_t *rawFilename = filename;
		if (strncmp(filename, "@", 1) == 0) {
		    isFileList = true;
		    rawFilename = filename + 1;
		} else if (strncmp(filename, "filelist://", 11) == 0) {
		    isFileList = true;
		    rawFilename = filename + 11;
		} else if (strncmp(filename, "filelist:", 9) == 0) {
		    isFileList = true;
		    rawFilename = filename + 9;
		} else if (strncmp(filename, "file://", 7) == 0) {
		    isFile = true;
		    rawFilename = filename + 7;
		} else if (strncmp(filename, "file:", 5) == 0) {
		    isFile = true;
		    rawFilename = filename + 5;
		} else if (strstr(filename, ".list") || strstr(filename, ".lis")) {
		    isFileList = true;
		    rawFilename = filename + 0;
		} else {
		    isFile = true;
		    rawFilename = filename + 0;
		}
		if (isFileList) {
			cout << "Reading file list " << rawFilename << endl;
			TString filenameExact = findFile(rawFilename);
			ifstream ifilelist(filenameExact);
			Int_t fileNum = 1;
			Char_t filenameBuf[1024];
			result = ifilelist.good();
			while (ifilelist.good() && !ifilelist.eof()) {
				ifilelist.getline(filenameBuf, sizeof(filenameBuf) - 1);
				if (!ifilelist.good() || ifilelist.eof()) break;
				if ((filenameBuf[0] == 0) || (filenameBuf[0] == '#') || (filenameBuf[0] == 0x0d) || (filenameBuf[0] == 0x0a)) continue;
				cout << "File " << fileNum << ", \"" << filenameBuf << "\"" << endl;
				result &= this->processFile(filenameBuf, outPath);
				fileNum++;
			}
		} else if (isFile) {
			cout << "File " << rawFilename;
			TString dataFilenameStr = findFile(rawFilename);
			const Char_t *dataFilename = dataFilenameStr;
			if (dataFilename) {
				Long_t id, size, flags, modtime;
				if (gSystem->GetPathInfo(dataFilename, &id, &size, &flags, &modtime) == 0) {
					TFile *dataFile = new TFile(dataFilename, "READ");
					if (dataFile && dataFile->IsOpen()) {
						cout << ": reading..." << endl;
						TFile *outFile = 0;
						if (outPath) {
						    TString inPathStr = dataFilename;
						    TObjArray *tokens = inPathStr.Tokenize(TString("/"));
						    if (tokens) {
							TObjString *lastStr = dynamic_cast<TObjString*>(tokens->Last());
							if (lastStr) {
							    TString outFilename = lastStr->GetString();
							    if (gSystem->GetPathInfo(outPath, &id, &size, &flags, &modtime) == 0) {
								outFilename.Prepend(outPath);
								cout << "Writing filtered data to " << outFilename.Data() << endl;
								outFile = new TFile(outFilename.Data(), "RECREATE");
								if (outFile && (!outFile->IsOpen())) {
								    delete outFile;
								    outFile = 0;
								}
							    } else {
								cout << "Output directory " << outFilename.Data() << " does not exist!" << endl;
							    }
							}
							delete tokens;
						    }
						}
						if (outFile) outFile->SetCompressionLevel(this->compression);
						dataFile->cd();
						result = this->processFile(dataFile, outFile);
						if (outFile) {
						    outFile->Close();
						    delete outFile;
						}
					} else cout << " not opened" << endl;
					if (dataFile) {
					    dataFile->Close();
					    delete dataFile;
					    dataFile = 0;
					}
				} else cout << " does not exist" << endl;
			} else cout << " ???" << endl;
		}
	}
	if (this->debug) cout << "TDataProcessorPool::processFile finished: " << result << endl;
	return result;
}

Bool_t TDataProcessorPool::writeToFile(TFile &outFile) const {
	if (this->debug) cout << "TDataProcessorPool::writeToFile started" << endl;
	Bool_t result = true;
	outFile.cd();
	cout << "Saving into file " << outFile.GetName() << endl;
	outFile.WriteObject(&this->settings, "settings");
	for (list_type::const_iterator iter = this->processors.begin();iter != this->processors.end();++iter) {
		processor_type *processor = const_cast<processor_type*>(*iter);
		if (processor) {
		    if (this->debug) cout << "Saving processor " << processor->GetName() << endl;
		    processor->Write();
		}
	}
	if (this->debug) cout << "TDataProcessorPool::writeToFile finished: " << result << endl;
	return result;
}

Bool_t TDataProcessorPool::writeToFile(const Char_t *filename) const {
	if (this->debug) cout << "TDataProcessorPool::writeToFile started: " << (filename ? filename : "") << endl;
	Bool_t result = true;
	if (filename){
		if (*filename) {
			TFile file(filename, "RECREATE");
			if (file.IsOpen()) {
				if (this->debug) cout << "File opened" << endl;
				result &= this->writeToFile(file);
				if (this->debug) cout << "Data processors written" << endl;
			} else {
				if (this->debug) cout << "Cannot recreate file " << filename << endl;
			}
		}
	}
	if (this->debug) cout << "TDataProcessorPool::writeToFile finished: " << result << endl;
	return result;
}

