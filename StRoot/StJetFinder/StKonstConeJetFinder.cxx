//!#include "stdafx.h"

//StKonstJetFinder.cxx
//adapted from M.L. Miller (Yale Software)
//07/02
//Thomas Henry
//08/02

//std
#include "Stiostream.h"
#include <algorithm>
#include <time.h>
using std::for_each;
using std::sort;

//StJetFinder
#include "StJetEtCell.h"
#include "StKonstConeJetFinder.h"
#include "StProtoJet.h"

ClassImp(StKonstConeJetFinder)

StKonstConeJetFinder::~StKonstConeJetFinder() { }
