// $Id: StTestData.h,v 1.3 2008/05/09 02:14:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STTESTDATA_H
#define STTESTDATA_H

#include <Rtypes.h>

struct TestParticle_t {
  Int_t     eventID;
  Char_t    name[128];
  Double_t  px;
  Double_t  py;
  Double_t  pz;
  Double_t  phi;
  Double_t  eta;
  Double_t  eT;
  Double_t  e;
  Double_t  mass;
  Double_t  charge;
};


#endif // STTESTDATA_H
