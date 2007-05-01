#ifndef StPi0Analysis_TDataPreloader
#define StPi0Analysis_TDataPreloader

#include <list>
using std::list;

#include <TNamed.h>
#include <TString.h>
#include <TThread.h>

class TFile;

#include "StPi0AnalysisVersion.h"

class TDataPreloader : public TNamed {
public:
    typedef TDataPreloader this_type;
    typedef TNamed inherited;
    typedef TFile file_type;
    typedef list<file_type*> file_list_type;
    typedef TThread thread_type;
    
protected:
    file_list_type mFiles;
    void clean();

    thread_type *mThread;

public:
    TDataPreloader(const Char_t *name = 0, const Char_t *title = 0);
    virtual ~TDataPreloader();

    virtual void Print(Option_t* option) const;

    Int_t debug;

    const Char_t *filelist;
    Int_t preloadFiles;
    file_list_type &getFiles() {return mFiles;}
    thread_type *getThread() {return mThread;}
    
    void openFilelist(const Char_t *filelist);
    void closeFilelist();

    file_type *openFile(const Char_t *filename);
    void closeFile(file_type *file);

    ClassDef(TDataPreloader, STPI0ANALYSIS_VERSION)
};

#endif
