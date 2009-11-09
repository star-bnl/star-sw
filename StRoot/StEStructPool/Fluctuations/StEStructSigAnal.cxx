
#include "StEStructSigAnal.h"
#include "StEStructSigAnal.h"

#include "TH2F.h"
#include "TFile.h"


ClassImp(StEStructSigAnal)

//--------------------------------------------------------------------------
StEStructSigAnal::StEStructSigAnal( const char *inputFile, const char *prefix ) {
  printf("Creating StEStructSigAnal object.\n");
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
void StEStructSigAnal::getLimits() {
    // get number of eta and phi bins from histogram called "nBins".
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    mNPhiBins = hnBins->GetNbinsX();
    mNEtaBins = hnBins->GetNbinsY();
    // get number of files. This assumes one bin for largest scale.
    mNFiles = hnBins->GetBinContent(mNPhiBins,mNEtaBins);
    // get minimum and maximum eta values from histogram called "EtaLimits".
    TH1F *hEtaLimits   = (TH1F *) gDirectory->Get("EtaLimits");
    mEtaMin = hEtaLimits->GetBinContent(1) / mNFiles;
    mEtaMax = hEtaLimits->GetBinContent(2) / mNFiles;
}
void StEStructSigAnal::normalizeCounters() {
    getLimits();
    TH2F *hnBins   = (TH2F *) gDirectory->Get("nBins");
    hnBins->Scale( 1.0/mNFiles );
    TH2F *hoffset = (TH2F *) gDirectory->Get("offset");
    hoffset->Scale( 1.0/mNFiles );
    TH2F *hfUnique = (TH2F *) gDirectory->Get("fUnique");
    hfUnique->Scale( 1.0/mNFiles );
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
printf("Scanning for centrality bins.\n");
    mnCents = 0;
    for (int i=0;i<100;i++) {
        sprintf( hist, "NSum%i_0", i );
        if (gDirectory->Get(hist)) {
            mnCents++;
        }
    }
printf("Found %i centrality bins. \n", mnCents);
    mnPts = 0;
    for (int i=0;i<100;i++) {
        sprintf( hist, "%sNSum0_%i_0", mpreFix, i );
        if (gDirectory->Get(hist)) {
            mnPts++;
        }
    }
printf("Found %i Pt bins. \n", mnPts);
    mnPtCents = 0;
    for (int i=0;i<100;i++) {
        sprintf( hist, "%sNSum%i_0_0", mpreFix, i );
        if (gDirectory->Get(hist)) {
            mnPtCents++;
        }
    }
printf("Found %i Pt centrality bins. \n", mnPtCents);

    // Want our histograms to stay around when we close mInputFile and
    // open a new one.
    delete mInFile;

    mnSigmas = mnCents + mnPts*mnPtCents;
    mSigma = new StEStructSigmas*[mnSigmas]; 
    char key[1024];
    for (int i=0;i<mnCents;i++) {
        sprintf(key,"%i",i);
        mSigma[i] = new StEStructSigmas(key,mNPhiBins,mNEtaBins,mEtaMin,mEtaMax);
    }
    int index = mnCents;
    for (int i=0;i<mnPtCents;i++) {
        for (int j=0;j<mnPts;j++) {
            sprintf(key,"%i_%i",i,j);
            mSigma[index] = new StEStructSigmas(key,mNPhiBins,mNEtaBins,
                                                    mEtaMin,mEtaMax,mpreFix);
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
