// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include <StjRunJetFinder.h>

#include <StjFourVecPrint.h>
#include <StjJetPrint.h>

#include <StProtoJet.h>
#include <StConePars.h>
#include <StConeJetFinder.h>

#include <TTree.h>

#include <iostream>
#include <string>

#include "RunJetFinderTest.hh"

using namespace std;

// Registers the fixture into the 'registry'
CPPUNIT_TEST_SUITE_REGISTRATION( RunJetFinderTest );

void RunJetFinderTest::setUp() 
{

}

void RunJetFinderTest::tearDown()
{

}

void RunJetFinderTest::testRun()
{
  StConePars* cpars = new StConePars();
  cpars->setGridSpacing(56, -1.6, 1.6, 120, -3.141592613589793, 3.141592613589793);
  cpars->setConeRadius(0.4);
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);
  StjRunJetFinder jetFinder;
  jetFinder.Init(cpars);

  StjFourVecList fourList = createFourVecList();

  StjFourVecPrint fourprint;
  //  fourprint(fourList);

  StjJetList jetList = jetFinder(fourList);

  StjJetPrint jetprint;
  jetprint(jetList);

  StjJetList expectedJetList = createJetList();

  CPPUNIT_ASSERT_EQUAL( expectedJetList, jetList);

}

void RunJetFinderTest::testRunRepeat()
{
  StConePars* cpars = new StConePars();
  cpars->setGridSpacing(56, -1.6, 1.6, 120, -3.141592613589793, 3.141592613589793);
  cpars->setConeRadius(0.4);
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);
  StjRunJetFinder jetFinder;
  jetFinder.Init(cpars);

  StjFourVecList fourList = createFourVecList();

  StjFourVecPrint fourprint;
  //  fourprint(fourList);

  //  while(1) jetFinder(fourList);

  StjJetList jetList = jetFinder(fourList);

  StjJetPrint jetprint;
  jetprint(jetList);

  StjJetList expectedJetList = createJetList();

  CPPUNIT_ASSERT_EQUAL( expectedJetList, jetList);

}

StjFourVecList RunJetFinderTest::createFourVecList()
{
  StjFourVecList ret;

  ret.push_back(createFourVec(6143024,   18358,    1, 1, 1, 2799,    0, 0,   0.923283,   0.724552,   0.874879,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    2, 1, 1, 2798,    0, 0,    1.20774,   0.290082,     1.3077,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    3, 1, 1, 2797,    0, 0,   0.747889,  -0.342101,    1.43174,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    4, 1, 1, 2796,    0, 0,   0.553708,   0.796128,    1.53265,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    5, 1, 1, 2795,    0, 0,    1.00236,   0.993059,    1.56782,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    6, 1, 1, 2787,    0, 0,    0.33272,   0.905262,   -1.73633,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    7, 1, 1, 2785,    0, 0,   0.467436,   0.587571,   -1.06465,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    8, 1, 1, 2784,    0, 0,    1.14746,    1.09041,    -1.3133,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,    9, 1, 1, 2782,    0, 0,   0.302314,    0.77667,   -1.56462,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   10, 1, 1, 2780,    0, 0,    1.21214,   0.883992,  -0.998302,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   11, 1, 1, 2774,    0, 0,   0.289968,   0.339625,     1.8854,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   12, 1, 1, 2770,    0, 0,   0.749084,   0.124089,    2.32001,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   13, 1, 1, 2759,    0, 0,   0.335719,   -0.36807,   -1.26502,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   14, 1, 1, 2758,    0, 0,    0.31488,    1.43786,    1.77108,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   15, 1, 1, 2754,    0, 0,   0.373382,  -0.614185,  -0.557547,        0.13957,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   16, 2, 9,    0,  181, 0,  0.0615938,    0.52526,   0.810193,      4.969e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   17, 2, 9,    0,  193, 0,  0.0400255,    1.00946,   0.810193,   -5.45648e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   18, 2, 9,    0,  247, 0,   0.106678,   0.774646,   0.650698,   -2.53721e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   19, 2, 9,    0,  402, 0,  0.0793278,   0.567974,   0.233078,   -3.31835e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   20, 2, 9,    0,  788, 0,  0.0128473,   0.814254,  -0.755653,    2.01506e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   21, 2, 9,    0,  829, 0,  0.0154157,   0.853815,  -0.860093,   -3.34021e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   22, 2, 9,    0,  877, 0,  0.0327906,    1.16356,   -0.96456,    9.34727e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   23, 2, 9,    0,  892, 0,  0.0259874,   0.970873,    -1.0196,   -4.64523e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   24, 2, 9,    0,  893, 0,   0.635404,    1.00949,    -1.0196,   -5.61696e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   25, 2, 9,    0,  894, 0,  0.0131186,    1.04804,    -1.0196,    3.18401e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   26, 2, 9,    0,  913, 0,   0.031355,     1.0096,   -1.06906,   -6.75999e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   27, 2, 9,    0,  930, 0,   0.191831,   0.893451,   -1.12411,    4.76592e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   28, 2, 9,    0,  931, 0,   0.218992,   0.932361,   -1.12411,     1.9129e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   29, 2, 9,    0,  932, 0,   0.107828,   0.971102,   -1.12411,      1.859e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   30, 2, 9,    0,  952, 0,  0.0888896,   0.971216,   -1.17358,     1.9978e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   31, 2, 9,    0,  980, 0,  0.0352992,    1.27669,   -1.22865,   -9.80717e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   32, 2, 9,    0,  999, 0,   0.063427,     1.2418,   -1.27815,    1.49538e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   33, 2, 9,    0, 1000, 0,    1.32632,    1.27683,   -1.27815,    1.28205e-08,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   34, 2, 9,    0, 1015, 0,    0.14217,    1.08734,   -1.33323,   -2.47023e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   35, 2, 9,    0, 1016, 0,   0.181738,    1.12589,   -1.33323,   -3.02568e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   36, 2, 9,    0, 1020, 0,  0.0562451,    1.27698,   -1.33323,    8.43509e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   37, 2, 9,    0, 1043, 0,   0.289149,   0.611852,   -1.43785,   -1.97906e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   38, 2, 9,    0, 1055, 0,   0.430705,    1.08762,   -1.43785,    2.09548e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   39, 2, 9,    0, 1073, 0,   0.068084,    1.01064,   -1.48738,   -1.74089e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   40, 2, 9,    0, 1074, 0,   0.134123,    1.04921,   -1.48738,   -9.42173e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   41, 2, 9,    0, 1076, 0,  0.0727401,    1.12632,   -1.48738,   -8.27286e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   42, 2, 9,    0, 1063, 0,   0.102148,   0.611946,   -1.48738,   -8.36957e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   43, 2, 9,    0, 1064, 0,   0.090681,   0.653812,   -1.48738,    9.19423e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   44, 2, 9,    0, 1093, 0,    1.14872,    1.01079,   -1.54251,   -1.45849e-08,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   45, 2, 9,    0, 1094, 0,   0.421368,    1.04936,   -1.54251,   -4.80557e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   46, 2, 9,    0, 1113, 0,  0.0251327,    1.01092,   -1.59205,   -3.33109e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   47, 2, 9,    0, 1341, 0,  0.0224743,    0.52721,   -2.22089,   -1.12484e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   48, 2, 9,    0, 1371, 0,   0.105216,    0.93518,   -2.27615,    1.68098e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   49, 2, 9,    0, 1392, 0,   0.231948,   0.974095,   -2.32582,    2.96116e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   50, 2, 9,    0, 1788, 0,   0.109902,   0.818492,    2.90688,    1.24502e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   51, 2, 9,    0, 1919, 0,  0.0194559,    1.24563,    2.59166,   -2.52047e-11,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   52, 2, 9,    0, 2023, 0,   0.012137,   0.613679,    2.27659,   -1.81535e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   53, 2, 9,    0, 2086, 0,   0.078437,   0.737449,    2.11634,   -1.03308e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   54, 2, 9,    0, 2189, 0,   0.135158,   0.856501,    1.85689,     -9.529e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   55, 2, 9,    0, 2230, 0,   0.168227,   0.895446,    1.75205,   -2.34859e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   56, 2, 9,    0, 2232, 0,   0.146151,   0.973198,    1.75205,    1.15832e-09,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   57, 2, 9,    0, 2233, 0,   0.574076,    1.01187,    1.75205,   -1.38236e-08,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   58, 2, 9,    0, 2234, 0,  0.0122867,    1.05047,    1.75205,   -1.22401e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   59, 2, 9,    0, 2251, 0,  0.0124482,   0.934269,    1.69685,    2.21768e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   60, 2, 9,    0, 2252, 0,  0.0231807,   0.973056,    1.69685,    3.58225e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   61, 2, 9,    0, 2293, 0,   0.107901,    1.01144,    1.59207,    3.74977e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   62, 2, 9,    0, 2299, 0,  0.0104751,    1.24329,    1.59207,    1.79408e-10,    -118.55));
  ret.push_back(createFourVec(6143024,   18358,   63, 2, 9,    0, 2322, 0,   0.032346,   0.569719,    1.48732,   -2.89762e-10,    -118.55));


  return ret;
}

StjFourVec RunJetFinderTest::createFourVec(int runNumber, int eventId,
					   int fourvecId, int type, int detectorId,  short trackId, int towerId, int mcparticleId,
					   double pt, double eta, double phi, double m,
					   double vertexZ)
{
  StjFourVec ret;
  ret.runNumber    = runNumber;
  ret.eventId      = eventId;
  ret.fourvecId    = fourvecId;
  ret.type         = type;
  ret.detectorId   = detectorId;
  ret.trackId      = trackId;
  ret.towerId      = towerId;       
  ret.mcparticleId = mcparticleId;       
  ret.pt           = pt;
  ret.eta          = eta;
  ret.phi          = phi;
  ret.m            = m;
  ret.vertexZ      = vertexZ;   
  return ret;
}

StjJetList RunJetFinderTest::createJetList()
{
  StjJetList ret;
  StjFourVecList fourveclist;

  fourveclist.push_back(createFourVec(6143024,   18358,    9, 1, 1, 2782,    0, 0,  0.302314,    0.77667,   -1.56462,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   27, 2, 9,    0,  930, 0,  0.191831,   0.893451,   -1.12411,    4.76592e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   10, 1, 1, 2780,    0, 0,   1.21214,   0.883992,  -0.998302,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   28, 2, 9,    0,  931, 0,  0.218992,   0.932361,   -1.12411,     1.9129e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   29, 2, 9,    0,  932, 0,  0.107828,   0.971102,   -1.12411,      1.859e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   44, 2, 9,    0, 1093, 0,   1.14872,    1.01079,   -1.54251,   -1.45849e-08,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   24, 2, 9,    0,  893, 0,  0.635404,    1.00949,    -1.0196,   -5.61696e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   45, 2, 9,    0, 1094, 0,  0.421368,    1.04936,   -1.54251,   -4.80557e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   40, 2, 9,    0, 1074, 0,  0.134123,    1.04921,   -1.48738,   -9.42173e-10,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   38, 2, 9,    0, 1055, 0,  0.430705,    1.08762,   -1.43785,    2.09548e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,    8, 1, 1, 2784,    0, 0,   1.14746,    1.09041,    -1.3133,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   34, 2, 9,    0, 1015, 0,   0.14217,    1.08734,   -1.33323,   -2.47023e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   35, 2, 9,    0, 1016, 0,  0.181738,    1.12589,   -1.33323,   -3.02568e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   33, 2, 9,    0, 1000, 0,   1.32632,    1.27683,   -1.27815,    1.28205e-08,    -118.55));
  ret.push_back(createJet(6143024,   18358,    1,    7.45059,    1.06812,   -1.28799,        2.04423,    0.647659, -118.55,   0.693636, fourveclist));

  fourveclist.clear();

  fourveclist.push_back(createFourVec(6143024,   18358,    4, 1, 1, 2796,    0, 0,  0.553708,   0.796128,    1.53265,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   54, 2, 9,    0, 2189, 0,  0.135158,   0.856501,    1.85689,     -9.529e-10,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   55, 2, 9,    0, 2230, 0,  0.168227,   0.895446,    1.75205,   -2.34859e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,    5, 1, 1, 2795,    0, 0,   1.00236,   0.993059,    1.56782,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   61, 2, 9,    0, 2293, 0,  0.107901,    1.01144,    1.59207,    3.74977e-10,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   56, 2, 9,    0, 2232, 0,  0.146151,   0.973198,    1.75205,    1.15832e-09,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   57, 2, 9,    0, 2233, 0,  0.574076,    1.01187,    1.75205,   -1.38236e-08,    -118.55));

  ret.push_back(createJet(6143024,   18358,    2,    2.67293,   0.949761,    1.63687,       0.534153,    0.418847, -118.55,   0.539992, fourveclist));

  fourveclist.clear();

  fourveclist.push_back(createFourVec(6143024,   18358,    3, 1, 1, 2797,    0, 0,  0.747889,  -0.342101,    1.43174,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,    2, 1, 1, 2798,    0, 0,   1.20774,   0.290082,     1.3077,        0.13957,    -118.55));
  ret.push_back(createJet(6143024,   18358,    3,    1.95208,  0.0483421,    1.35512,       0.684942,    0.0, -118.55,  -0.466989, fourveclist));

  fourveclist.clear();

  fourveclist.push_back(createFourVec(6143024,   18358,    1, 1, 1, 2799,    0, 0,  0.923283,   0.724552,   0.874879,        0.13957,    -118.55));
  fourveclist.push_back(createFourVec(6143024,   18358,   18, 2, 9,    0,  247, 0,  0.106678,   0.774646,   0.650698,   -2.53721e-10,    -118.55));
  ret.push_back(createJet(6143024,   18358,    4,    1.02757,   0.731266,   0.851798,       0.164231,    0.102928, -118.55,   0.262665, fourveclist));

  fourveclist.clear();

  fourveclist.push_back(createFourVec(6143024,   18358,   12, 1, 1, 2770,    0, 0,  0.749084,   0.124089,    2.32001,        0.13957,    -118.55));
  ret.push_back(createJet(6143024,   18358,    5,   0.749084,   0.124089,    2.32001,        0.13957,    0.0, -118.55,  -0.397547, fourveclist));

  return ret;
}

StjJet RunJetFinderTest::createJet(int runNumber, int eventId,
				   int jetId,
				   double pt, double eta, double phi, double m,
				   double neuRt,
				   double vertexZ, double detectorEta,
				   const StjFourVecList&    fourVecList)
{
  StjJet ret;
  ret.runNumber = runNumber;  
  ret.eventId = eventId;   
  ret.jetId  = jetId;   
  ret.pt    = pt;   
  ret.eta     = eta;   
  ret.phi       = phi;   
  ret.m          = m;   
  ret.neuRt       = neuRt;
  ret.vertexZ     = vertexZ;   
  ret.detectorEta = detectorEta;
  ret.fourVecList = fourVecList;
  return ret;
}
