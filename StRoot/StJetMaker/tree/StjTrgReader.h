// -*- mode: c++;-*-
// $Id: StjTrgReader.h,v 1.1 2008/08/11 06:07:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGREADER_H
#define STJTRGREADER_H

#include "StjTreeReader.h"

#include <Rtypes.h>

class StjTrgReader : public StjTreeReader {

public:
  StjTrgReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjTrgReader() { }

  int              id()         const { return __id;         }
  int              runNumber()  const { return __runNumber;  }
  int              eventId()    const { return __eventId;    }
  bool             hard()       const { return __hard;       }
  bool             soft()       const { return __soft;       }
  bool             pass()       const { return __pass;       }
  double           prescale()   const { return __prescale;   }
  double           vertexZ()    const { return __vertexZ;    }
  std::vector<int> towers()     const { return __towers;     }
  std::vector<int> jetPatches() const { return __jetPatches; }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  Int_t    _runNumber;
  Int_t    _eventId;
  Double_t _vertexZ;
  Int_t    _trigID;
  Double_t _prescale;
  Int_t    _passed;
  Int_t    _hard;
  Int_t    _soft;
  Int_t    _nTowers;
  Int_t    _towerId[4800];
  Int_t    _nJetPatches;
  Int_t    _jetPatchId[12];

  int              __id;
  int              __runNumber;
  int              __eventId;
  bool             __hard;
  bool             __soft;
  bool             __pass;
  double           __prescale;
  double           __vertexZ;
  std::vector<int> __towers;
  std::vector<int> __jetPatches;

  ClassDef(StjTrgReader, 1)

};

#endif // STJTRGREADER_H
