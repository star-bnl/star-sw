// -*- mode: c++;-*-
// $Id: StjSpinWriter.h,v 1.1 2008/11/05 05:48:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJSPINWRITER_H
#define STJSPINWRITER_H

#include <TObject.h>

#include <Rtypes.h>

#include <string>

class TDirectory;
class TTree;

class StjSpin;

class StjSpinWriter : public TObject {

public:
  StjSpinWriter(const char *treeName, const char* treeTitle,
		  TDirectory* file, StjSpin* spin)
    : _treeName(treeName), _treeTitle(treeName)
    , _file(file), _spin(spin)
  { }
  virtual ~StjSpinWriter() { }

  void Init();
  void Make();
  void Finish();

private:

  std::string _treeName;
  std::string _treeTitle;

  TDirectory* _file;
  TTree*      _tree;

  StjSpin* _spin;

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _bx7;
  Int_t _bx48;
  Int_t _spin4;
  Int_t _bbcTimebin;
  Double_t _vertexZ;

  ClassDef(StjSpinWriter, 1)

};

#endif // STJSPINWRITER_H
