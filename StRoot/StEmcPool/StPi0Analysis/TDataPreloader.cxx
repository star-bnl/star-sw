#include "TDataPreloader.h"

#include <TFile.h>
#include <TClass.h>
#include <TDirectory.h>
#include <TKey.h>

//#include <iostream>
#include <fstream>
using namespace std;

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
#include <StEmcPool/StPi0Common/Logger.h>

ClassImp(TDataPreloader)

TDataPreloader::TDataPreloader(const Char_t *name, const Char_t *title) 
    : inherited(name, title)
    , mThread(0)
    , debug(0)
    , filelist(0)
    , preloadFiles(0)
{

}

TDataPreloader::~TDataPreloader() {
    this->clean();
}

void TDataPreloader::Print(Option_t* option) const {
        const Char_t *prefix = option ? ((const Char_t *)option) : "";
        const Char_t *tab = "\t";
        cout << prefix << this->IsA()->GetName() << " " << this->GetName() << " \"" << this->GetTitle() << "\"" << endl;
        cout << prefix << tab << "debug = " << this->debug << endl;
        cout << prefix << tab << "filelist = " << this->filelist << endl;
        cout << prefix << tab << "preloadFiles = " << this->preloadFiles << endl;
//        TString newPrefix(prefix);
//        newPrefix += tab;
}

void TDataPreloader::clean() {
    this->filelist = 0;
    for (file_list_type::iterator iter = this->mFiles.begin();iter != this->mFiles.end();++iter) {
	file_type *file = *iter;
	if (file) {
	    file->Close();
	    delete file;
	}
    }
    this->mFiles.clear();
}

void threadFunction(void *arg) {
    TDataPreloader *pre = arg ? reinterpret_cast<TDataPreloader*>(arg) : 0;
    if (!pre) return;
    if (pre->debug) cout << "PRELOAD> Starting thread function" << endl;
    TDataPreloader::file_list_type &files = pre->getFiles();
    TDataPreloader::thread_type *thread = pre->getThread();
    TString filelistExact = findFile(pre->filelist);
    ifstream ifilelist(filelistExact);
    Char_t filenameBuf[1024];
    while (ifilelist.good() && !ifilelist.eof()) {
	//if (pre->debug) cout << "PRELOAD> locking thread" << endl;
	if (thread) thread->Lock();
	//if (pre->debug) cout << "PRELOAD> thread locked" << endl;
	if ((Int_t)files.size() < pre->preloadFiles) {
    	    ifilelist.getline(filenameBuf, sizeof(filenameBuf) - 1);
    	    if (ifilelist.good() && !ifilelist.eof()) {
    		if ((filenameBuf[0] == 0) || (filenameBuf[0] == '#') || (filenameBuf[0] == 0x0d) || (filenameBuf[0] == 0x0a)) {
    		} else {
		    if (pre->debug) cout << "PRELOAD> opening file " << filenameBuf << endl;
		    TDirectory *curDir = gDirectory;
		    TString filenameExact = findFile(filenameBuf);
		    TFile *file = new TFile(filenameExact, "READ");
		    //if (curDir) curDir->cd();
		    gDirectory = curDir;
		    if (file) {
            		TIter inIter(file->GetListOfKeys());
                        while(TKey *key = dynamic_cast<TKey*>(inIter())) {
                            /*TObject *obj = */key->ReadObj();
                        }
    			files.push_back(file);
			if (pre->debug) cout << "PRELOAD> file added " << file << endl;
		    }
		}
	    }
	}
	if (thread) thread->UnLock();
	//if (pre->debug) cout << "PRELOAD> thread unlocked, sleeping" << endl;
	sleep(1);
    }
    if (pre->debug) cout << "PRELOAD> Finished thread function" << endl;
}

void TDataPreloader::openFilelist(const Char_t *filelist) {
    if (this->debug) cout << "PRELOADER> openFilelist " << filelist << endl;
    this->filelist = filelist;
    this->mThread = new TThread(&threadFunction, (void*)this);
    if (this->mThread) {
	if (this->debug) cout << "PRELOADER> Running the thread..." << endl;
	this->mThread->Run((void*)this);
    }
}

void TDataPreloader::closeFilelist() {
    if (this->debug) cout << "PRELOADER> closeFilelist" << endl;
    if (this->mThread) {
	this->mThread->Kill();
	delete this->mThread;
	this->mThread = 0;
    }
    if (this->debug) cout << "PRELOADER> Thread killed." << endl;
    this->clean();
}

TDataPreloader::file_type *TDataPreloader::openFile(const Char_t *filename) {
    if (this->debug) cout << "PRELOADER> openFile " << filename << endl;
    file_type *file = 0;
    if (this->debug) cout << "PRELOADER> locking thread" << endl;
    if (this->mThread) this->mThread->Lock();
    if (this->debug) cout << "PRELOADER> Thread locked" << endl;
    for (file_list_type::iterator iter = this->mFiles.begin();(iter != this->mFiles.end()) && !file;++iter) {
        file_type *f = *iter;
        if (f) {
    	    if (strcmp(f->GetName(), filename) == 0) {
		file = f;
	    }
	}
    }
    if (!file) {
	TString filenameExact = findFile(filename);
	file = new TFile(filenameExact, "READ");
    }
    if (this->mThread) this->mThread->UnLock();
    if (this->debug) cout << "PRELOADER> Thread unlocked: file = " << file << endl;
    return file;
}

void TDataPreloader::closeFile(file_type *file) {
    if (this->debug) cout << "PRELOADER> closeFile " << file << endl;
    if (this->mThread) this->mThread->Lock();
    if (this->debug) cout << "PRELOADER> Thread locked" << endl;
    for (file_list_type::iterator iter = this->mFiles.begin();iter != this->mFiles.end();++iter) {
	file_type* f = *iter;
	if (f && (f == file)) {
	    this->mFiles.erase(iter);
	    --iter;
	}
    }
    file->Close();
    delete file;    
    if (this->mThread) this->mThread->UnLock();
    if (this->debug) cout << "PRELOADER> Thread unlocked" << endl;
}
