//StPythiaAssociator.h
//M.L. Miller (MIT)
//2/05

#ifndef StPythiaAssociator_HH
#define StPythiaAssociator_HH
#include "StMaker.h"
#include "StJetMaker.h"
#include "StJetReader.h"
#include <string>
using namespace std;

class TTree;
class TFile;
class IoManager;

class StPythiaAssociator : public StMaker
{
public:

    ///A useful typedef for the StJets map
    typedef StJetReader::JetBranchesMap JetBranchesMap;

    ///The constructor requires a valid instance of StMuDstMaker
    StPythiaAssociator(const char* maker_name, const char* infile_name, StJetReader* jreader);
    virtual ~StPythiaAssociator();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    
private:
    string mFilename;
    StJetReader* mReader;
    IoManager* mIoManager;
    
    ClassDef(StPythiaAssociator,1)
};

#endif
