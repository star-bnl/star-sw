// -*- mode: c++;-*-
// $Id: StjMCKinWriter.h,v 1.1 2008/08/22 22:10:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCKINWRITER_H
#define STJMCKINWRITER_H

#include <TObject.h>

#include <Rtypes.h>

#include <string>

class TDirectory;
class TTree;

class StjMCKin;

class StjMCKinWriter : public TObject {

public:
  StjMCKinWriter(const char *treeName, const char* treeTitle,
		  TDirectory* file, StjMCKin* mckin)
    : _treeName(treeName), _treeTitle(treeName)
    , _file(file), _mckin(mckin)
  { }
  virtual ~StjMCKinWriter() { }

  void Init();
  void Make();
  void Finish();

private:

  std::string _treeName;
  std::string _treeTitle;

  TDirectory* _file;
  TTree*      _tree;

  StjMCKin* _mckin;

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _s;
  Double_t _t;
  Double_t _u;
  Double_t _pt;
  Double_t _costh;
  Double_t _x1;
  Double_t _x2;
  Int_t    _pid;
  Double_t _vertexZ;

  ClassDef(StjMCKinWriter, 1)

};

#endif // STJMCKINWRITER_H
