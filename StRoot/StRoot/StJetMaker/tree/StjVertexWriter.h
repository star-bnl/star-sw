// -*- mode: c++;-*-
// $Id: StjVertexWriter.h,v 1.1 2008/08/13 19:37:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJVERTEXWRITER_H
#define STJVERTEXWRITER_H

#include <TObject.h>

#include <Rtypes.h>

#include <string>

class TDirectory;
class TTree;

class StjVertex;

class StjVertexWriter : public TObject {

public:
  StjVertexWriter(const char *treeName, const char* treeTitle,
		  TDirectory* file, StjVertex* vertex)
    : _treeName(treeName), _treeTitle(treeName)
    , _file(file), _vertex(vertex)
  { }
  virtual ~StjVertexWriter() { }

  void Init();
  void Make();
  void Finish();

private:

  std::string _treeName;
  std::string _treeTitle;

  TDirectory* _file;
  TTree*      _tree;

  StjVertex* _vertex;

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Double_t _vertexY;
  Double_t _vertexX;

  ClassDef(StjVertexWriter, 1)

};

#endif // STJVERTEXWRITER_H
