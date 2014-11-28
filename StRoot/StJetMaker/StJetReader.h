// -*- mode: c++ -*-
//StJetReader.h
//M.L. Miller
//MIT Software
//6/04

#ifndef STJETREADER_H
#define STJETREADER_H

class TTree;
class TFile;
class StJets;
class StJetSkimEvent;

#include "StMaker.h"

/*!
  \class StJetReader
  \author M.L. Miller (MIT Software)
  StJetReader is a utility maker that reads the jet tree from file.  Currently it only supports
  single file reading, but additions to multiple files and chain reading are planned.  Also,
  we are still debugging the InitTree() method, which would allow for the jet tree to be
  a "friend" of the StMuDst, and would therefore make the StMuDstMaker responsible for the reading
  from file (a great savings in effort and bookkeeping!).
  An example block of analysis code is given in the exampleEventAna() method.
*/

class StJetReader : public StMaker
{
public:
  ///The constructor requires a valid instance of StMuDstMaker
  StJetReader(const char* name = "JetReader");
  virtual ~StJetReader();
    
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
    
  ///Recover the TTree from file and prepare for reading
  virtual void InitFile(const char* file);
	
  ///Recover the "fast" tree of StJetSkimEvent
  virtual void InitJetSkimFile(const char* file);
    
  ///Prepare for reading the TTree, but as a "friend" of the StMuDst tree
  ///BUG: this doesn't work with MuDst since 2005 and on...
  virtual void InitTree(TTree* tree);
	
  ///Check if we are all ready to read the Skim and StjJet trees together
  int preparedForDualRead();
    
  ///Access to jet branches
  int numberOfBranches() const;
  const char* branchName(int i) const;
  StJets* getStJets(int i) const;
  StJets* getStJets(const char* bname) const;
    
  ///An example analysis method, look here for a demonstration of jet/track histogramming
  void exampleEventAna();

  ///An example analysis method to read StJetSkimEvent and StJets trees together
  void exampleFastAna();

  ///Access to the StJets tree
  TTree* tree() const {return mTree;}
	
  ///Access to the StJetSkimEvent tree
  TTree* skimTree() const {return mSkimTree;}
	
  ///Acces to the StJetSkimEvent
  StJetSkimEvent* skimEvent() const {return mSkimEvent;}

private:
  TFile* mFile;
  TTree* mTree;
  TFile* mSkimFile;
  TTree* mSkimTree;
  bool mValid; //!
  StJetSkimEvent* mSkimEvent;//!
  //temp, MLM
  ofstream* mOfstream; //!

  ClassDef(StJetReader,0);
};

#endif // STJETREADER_H
