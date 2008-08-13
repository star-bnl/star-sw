// -*- mode: c++;-*-
// $Id: StjVertexReader.h,v 1.1 2008/08/13 19:37:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJVERTEXREADER_H
#define STJVERTEXREADER_H

#include "StjTreeReader.h"

class StjVertexReader : public StjTreeReader {

public:
  StjVertexReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjVertexReader() { }

  int              runNumber()  const { return __runNumber;  }
  int              eventId()    const { return __eventId;    }
  double           vertexZ()    const { return __vertexZ;    }
  double           vertexY()    const { return __vertexY;    }
  double           vertexX()    const { return __vertexX;    }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Double_t _vertexY;
  Double_t _vertexX;

  int              __runNumber;
  int              __eventId;
  double           __vertexZ;
  double           __vertexY;
  double           __vertexX;

  ClassDef(StjVertexReader, 1)

};

#endif // STJVERTEXREADER_H
