//StJetSimuReader.h
//M.L. Miller
//MIT Software
//6/04

#ifndef StJetSimuReader_HH
#define StJetSimuReader_HH

#include "StMaker.h"
#include "StJetMaker/StJetMaker.h"

class TTree;
class TFile;
class StMuDstMaker;

/*!
  \class StJetSimuReader
  \author M.L. Miller (MIT Software)
  StJetSimuReader is a utility maker that reads the jet tree from file.  Currently it only supports
  single file reading, but additions to multiple files and chain reading are planned.  Also,
  we are still debugging the InitTree() method, which would allow for the jet tree to be
  a "friend" of the StMuDst, and would therefore make the StMuDstMaker responsible for the reading
  from file (a great savings in effort and bookkeeping!).
  An example block of analysis code is given in the exampleEventAna() method.
*/
class StJetSimuReader : public StMaker
{
public:

    ///A useful typedef for the StJets map
    typedef map<string, StJets*, less<string> > JetBranchesMap;

    ///The constructor requires a valid instance of StMuDstMaker
    StJetSimuReader(const char* name, StMuDstMaker* uDstMaker);
    virtual ~StJetSimuReader();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    
    ///Recover the TTree from file and prepare for reading
    virtual void InitFile(const char* file,const char *simufile);
    
    ///Prepare for reading the TTree, but as a "friend" of the StMuDst tree
    virtual void InitTree(TTree* tree);
    
    ///Access to the StJetsMap
    JetBranchesMap& jetsMap();
    
    ///An example analysis method, look here for a demonstration of jet/track histogramming
    void exampleEventAna();
    void exampleSimuAna();

private:
    JetBranchesMap mStJetsMap;
    TFile* mFile;
    TFile* sFile;
    TTree* mTree;
    TTree *stree;
    StMuDstMaker* mDstMaker;
    int mCounter;
    int EveNum;

    int pid;//subprocess id
    int BHTmax;//Barrel HT in event
    int BJPmax;//Barrel max JP in event
    int BJPsum;//Barrle JP sum in event
    int EHTmax;//Endcap HT in event
    int EJPmax;//Endcap max JP in event
    int EJPsum;//Endcap JP sum in event
    int BJP[6];//Barrel JP Sums
    int EJP[6];//EEMC JP Sums
    int bbc;//1 if BBC trigger is met
    int Badc[48];//BBC pmt adc
    
    
    //temp, MLM
    ofstream* mOfstream; //!

    
    ClassDef(StJetSimuReader,1)
	};

//inlines ---

inline StJetSimuReader::JetBranchesMap& StJetSimuReader::jetsMap()
{
    return mStJetsMap;
}


#endif
