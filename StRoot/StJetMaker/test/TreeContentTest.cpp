// Copyright (C) 2007 Tai Sakuma <sakuma@mit.edu>
#include "TreeContentTest.hh"

#include <TSystem.h>
#include <TFile.h>
#include <TTree.h>
#include <TObject.h>


#include <Stiostream.h>
#include <StMessMgr.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>
#include <StJetFinder/StProtoJet.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StFourPMakers/StMuEmcPosition.h>
#include <StMuTrackFourVec.h>
#include <StJet.h>
#include <StJets.h>

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( TreeContentTest );

void TreeContentTest::setUp() 
{

}

void TreeContentTest::tearDown() 
{

}

void TreeContentTest::testOne() 
{
//  TFile *expecptedFile = TFile::Open("./jets_7156024.tree.expected.root", "READ");
//  CPPUNIT_ASSERT( expecptedFile && expecptedFile->IsOpen() );
//
//  TTree *expectedTree = dynamic_cast<TTree*>(expecptedFile->Get("jet"));
//
//  StJets *stjets  = new StJets;
//
//  expectedTree->SetBranchAddress("ConeJets12", stjets);

}
