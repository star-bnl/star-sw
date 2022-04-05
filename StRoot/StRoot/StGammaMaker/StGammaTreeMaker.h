////////////////////////////////////////////////////////////
//                                                        //
//    StGammaTreeMaker                                    //
//                                                        //
//    Store StGammaEvents in a tree                       //
//                                                        //
//    Original concept and implementation by              //
//    Jason  Webb (Valpo)                                 //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaTreeMaker
#define STAR_StGammaTreeMaker

#include <vector>
#include "StMaker.h"
#include "TString.h"

class TTree;
class TFile;
class StGammaEvent;
class StGammaTreeMaker;

class StGammaTreeVersion: public TNamed
{

    public:
        StGammaTreeVersion(const char *name = "version", const char *title = "Versioning Info");
        ~StGammaTreeVersion();
        
        void print();
        
    private:
    protected:
        
        std::vector<TString> mMakerTags;   
        std::vector<TString> mStorageTags;
        
        friend class StGammaTreeMaker;
        
        ClassDef(StGammaTreeVersion, 2);

};

class StGammaTreeMaker: public StMaker
{

    public:
        
        StGammaTreeMaker(const char *name = "mGammaTreeMaker" );
        ~StGammaTreeMaker();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaTreeMaker.h,v 1.6 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        // Required Maker Methods
        Int_t Init();
        Int_t Make();
        void  Clear(Option_t *opts="");
        Int_t Finish();
        
        // Accessors
        TFile *file() { return mGammaFile; }
        TTree *tree() { return mGammaTree; }
        StGammaEvent *event() { return mGammaEvent; }
        
        // Mutators
        
        /// Sets the output file for the TTree.  Defaults into .hist branch
        void SetFile( TFile *file ) { mGammaFile = file; }
        /// Sets a pointer to an existing TTree.  Default creates at Init().
        void SetTree( TTree *tree ) { mGammaTree = tree; }
        /// Sets the filename (unless a file has been passed above)
        void SetFilename( const Char_t *fname ) { mFilename = fname; }
        
        void skipEmptyEvents() { mSkipEmpty = true; }
        void storeEmptyEvents() { mSkipEmpty = false; }

    private:

        bool mSkipEmpty;

        TTree *mGammaTree;
        TFile *mGammaFile;
        
        StGammaEvent *mGammaEvent;
        TString mFilename;
        
        StGammaTreeVersion mVersion;
        
        ClassDef(StGammaTreeMaker, 2);

};

#endif
