
#include "StEStructSigAnal.h"
#include "StEStructSigAnal.h"

#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructSigAnal)

//--------------------------------------------------------------------------
StEStructSigAnal::StEStructSigAnal( char *inputFile, char *prefix ) {
  cout << "Creating StEStructSigAnal object." << endl;
    mInputFile = strdup( inputFile );
    mpreFix    = strdup( prefix );
    initArrays();
}
//--------------------------------------------------------------------------
StEStructSigAnal::~StEStructSigAnal() {
    free( mInputFile );
    free( mpreFix );
    deleteArrays();
}
//--------------------------------------------------------------------------
void StEStructSigAnal::newFile( char *inputFile ) {
    if (mInputFile) {
        free( mInputFile );
    }
    if (mInFile) {
        delete mInFile;
    }
    mInputFile = strdup( inputFile );
    mInFile = new TFile( mInputFile );
    normalizeCounters();
}
//--------------------------------------------------------------------------
void StEStructSigAnal::closeFile() {
    if (mInputFile) {
        free( mInputFile );
    }
    if (mInFile) {
        delete mInFile;
    }
    mInputFile = 0;
    mInFile = 0;
}
//--------------------------------------------------------------------------
void StEStructSigAnal::normalizeCounters() {
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    mNPhiBins = hnBins->GetNbinsX();
    mNEtaBins = hnBins->GetNbinsY();
    float nFiles = hnBins->GetBinContent(mNPhiBins,mNEtaBins);
    hnBins->Scale( 1.0/nFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/nFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/nFiles );
}
//--------------------------------------------------------------------------
void StEStructSigAnal::fillHistograms() {
    for (int i=0;i<mnSigmas;i++) {
        mSigma[i]->fillHistograms();
    }
}
void StEStructSigAnal::writeHistograms(TFile* sig) {

    sig->cd();
    for (int i=0;i<mnSigmas;i++) {
        mSigma[i]->writeHistograms();
    }
}
//--------------------------------------------------------------------------
void StEStructSigAnal::initArrays() {

    // There should be a more clever way to extract the pt/centrality keys,
    // but I can't come up with one quickly.
    // Assume we have fewer than 20 centrality bins, fewer then 10 pt
    // bins and fewer than 10 centrality bins for every pt bin.
    // Every Pt bin has same number of centrality bins.
    char hist[1024];

    mInFile = new TFile( mInputFile );
    normalizeCounters();
cout << "Scanning for centrality bins." << endl;
    mnCents = 0;
    for (int i=0;i<20;i++) {
        sprintf( hist, "NSum%i_0", i );
        if (gDirectory->Get(hist)) {
            mnCents++;
        }
    }
cout << "Found " << mnCents << " centrality bins. " << endl;
    mnPts = 0;
    for (int i=0;i<10;i++) {
        sprintf( hist, "%sNSum0_%i_0", mpreFix, i );
        if (gDirectory->Get(hist)) {
            mnPts++;
        }
    }
cout << "Found " << mnPts << " Pt bins. " << endl;
    mnPtCents = 0;
    for (int i=0;i<10;i++) {
        sprintf( hist, "%sNSum%i_0_0", mpreFix, i );
        if (gDirectory->Get(hist)) {
            mnPtCents++;
        }
    }
cout << "Found " << mnPtCents << " Pt centrality bins. " << endl;

    // Want our histograms to stay around when we close mInputFile and
    // open a new one.
    delete mInFile;

    mnSigmas = mnCents + mnPts*mnPtCents;
    mSigma = new StEStructSigmas*[mnSigmas]; 
    char key[1024];
    for (int i=0;i<mnCents;i++) {
        sprintf(key,"%i",i);
        mSigma[i] = new StEStructSigmas(key,mNPhiBins,mNEtaBins);
    }
    int index = mnCents;
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            sprintf(key,"%i_%i",i,j);
            mSigma[index] = new StEStructSigmas(key,mNPhiBins,mNEtaBins,mpreFix);
            index++;
        }
    }
    mInFile = new TFile( mInputFile );
    normalizeCounters();
}
//--------------------------------------------------------------------------
void StEStructSigAnal::deleteArrays() {
    for (int i=0;i<mnSigmas;i++) {
        delete mSigma[i];
    }
}
