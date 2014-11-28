// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjTrgReader.h>
#include <StjTrgTree.h>

#include <TFile.h>
#include <TTree.h>

#include "StjTrgTreeTest.hh"

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( StjTrgTreeTest );

void StjTrgTreeTest::setUp() 
{

}

void StjTrgTreeTest::tearDown() 
{

}

void StjTrgTreeTest::testOne()
{
  TFile* file = new TFile("./part_run6143024.root");

  TTree *tree = dynamic_cast<TTree*>(file->Get("trgBJP2"));

  StjTrgReader *reader = new StjTrgReader(tree);

  tree->BuildIndex("runNumber", "eventId");

  reader->Init();

  StjTrgTree* trg = new StjTrgTree(reader);

  reader->GetEntryWithIndex(6143024, 1095);
  CPPUNIT_ASSERT_EQUAL(   96233,  trg->id()        );
  CPPUNIT_ASSERT_EQUAL( 6143024,  trg->runNumber() );
  CPPUNIT_ASSERT_EQUAL(    1095,  trg->eventId()   );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->hard()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->soft()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->passed()      );
  CPPUNIT_ASSERT_EQUAL(     1.0,  trg->prescale()  );
  CPPUNIT_ASSERT_EQUAL( (size_t)0,  trg->towers().size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1,  trg->jetPatches().size() );
  CPPUNIT_ASSERT_EQUAL(         3,  trg->jetPatches()[0] );

  reader->GetEntryWithIndex(6143024, 2000);
  CPPUNIT_ASSERT_EQUAL(   false,  trg->hard()      );
  CPPUNIT_ASSERT_EQUAL(   false,  trg->soft()      );
  CPPUNIT_ASSERT_EQUAL(   false,  trg->passed()      );
  CPPUNIT_ASSERT_EQUAL( (size_t)0,  trg->towers().size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)0,  trg->jetPatches().size() );

  reader->GetEntryWithIndex(6143024, 24897);
  CPPUNIT_ASSERT_EQUAL(   96233,  trg->id()        );
  CPPUNIT_ASSERT_EQUAL( 6143024,  trg->runNumber() );
  CPPUNIT_ASSERT_EQUAL(   24897,  trg->eventId()   );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->hard()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->soft()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->passed()      );
  CPPUNIT_ASSERT_EQUAL(     1.0,  trg->prescale()  );
  CPPUNIT_ASSERT_EQUAL( (size_t)0,  trg->towers().size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1,  trg->jetPatches().size() );
  CPPUNIT_ASSERT_EQUAL(         4,  trg->jetPatches()[0] );

  reader->GetEntryWithIndex(6143024, 28661);
  CPPUNIT_ASSERT_EQUAL(   96233,  trg->id()        );
  CPPUNIT_ASSERT_EQUAL( 6143024,  trg->runNumber() );
  CPPUNIT_ASSERT_EQUAL(   28661,  trg->eventId()   );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->hard()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->soft()      );
  CPPUNIT_ASSERT_EQUAL(    true,  trg->passed()      );
  CPPUNIT_ASSERT_EQUAL(     1.0,  trg->prescale()  );
  CPPUNIT_ASSERT_EQUAL( (size_t)0,  trg->towers().size() );
  CPPUNIT_ASSERT_EQUAL( (size_t)1,  trg->jetPatches().size() );
  CPPUNIT_ASSERT_EQUAL(         2,  trg->jetPatches()[0] );

}
