//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StFastJetPars.cxx,v 1.4 2016/01/06 22:00:17 gdwebb Exp $
//
// $Log: StFastJetPars.cxx,v $
// Revision 1.4  2016/01/06 22:00:17  gdwebb
// This is code to implement the off axis cone underlying event analysis.
//
// Revision 1.3  2012/03/10 23:18:28  pibero
// Added destructor for StCDFMidPointPlugin
//
// Revision 1.2  2012/03/10 23:09:53  pibero
// Addeed support for fastjet plugins
//
// Revision 1.1  2011/08/31 17:57:44  pibero
// Support for FastJet
//
// http://www.lpthe.jussieu.fr/~salam/fastjet/
// http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8521.html
//
//

#include "fastjet/JetDefinition.hh"
#include "StjFastJet.h"
#include "StFastJetPars.h"

// enum JetAlgorithm
const int StFastJetPars::kt_algorithm                    = fastjet::kt_algorithm;
const int StFastJetPars::cambridge_algorithm             = fastjet::cambridge_algorithm;
const int StFastJetPars::antikt_algorithm                = fastjet::antikt_algorithm;
const int StFastJetPars::genkt_algorithm                 = fastjet::genkt_algorithm;
const int StFastJetPars::cambridge_for_passive_algorithm = fastjet::cambridge_for_passive_algorithm;
const int StFastJetPars::genkt_for_passive_algorithm     = fastjet::genkt_for_passive_algorithm;
const int StFastJetPars::ee_kt_algorithm                 = fastjet::ee_kt_algorithm;
const int StFastJetPars::ee_genkt_algorithm              = fastjet::ee_genkt_algorithm;
const int StFastJetPars::plugin_algorithm                = fastjet::plugin_algorithm;

// enum RecombinationScheme
const int StFastJetPars::E_scheme        = fastjet::E_scheme;
const int StFastJetPars::pt_scheme       = fastjet::pt_scheme;
const int StFastJetPars::pt2_scheme      = fastjet::pt2_scheme;
const int StFastJetPars::Et_scheme       = fastjet::Et_scheme;
const int StFastJetPars::Et2_scheme      = fastjet::Et2_scheme;
const int StFastJetPars::BIpt_scheme     = fastjet::BIpt_scheme;
const int StFastJetPars::BIpt2_scheme    = fastjet::BIpt2_scheme;
const int StFastJetPars::external_scheme = fastjet::external_scheme;

// enum Strategy
const int StFastJetPars::N2MinHeapTiled  = fastjet::N2MinHeapTiled;
const int StFastJetPars::N2Tiled         = fastjet::N2Tiled;
const int StFastJetPars::N2PoorTiled     = fastjet::N2PoorTiled;
const int StFastJetPars::N2Plain         = fastjet::N2Plain;
const int StFastJetPars::N3Dumb          = fastjet::N3Dumb;
const int StFastJetPars::Best            = fastjet::Best;
const int StFastJetPars::NlnN            = fastjet::NlnN;
const int StFastJetPars::NlnN3pi         = fastjet::NlnN3pi;
const int StFastJetPars::NlnN4pi         = fastjet::NlnN4pi;
const int StFastJetPars::NlnNCam4pi      = fastjet::NlnNCam4pi;
const int StFastJetPars::NlnNCam2pi2R    = fastjet::NlnNCam2pi2R;
const int StFastJetPars::NlnNCam         = fastjet::NlnNCam;
const int StFastJetPars::plugin_strategy = fastjet::plugin_strategy;

ClassImp(StFastJetPars)

StJetFinder* StFastJetPars::constructJetFinder() { return new StjFastJet(*this); }

#include "fastjet/CDFMidPointPlugin.hh"

ClassImp(StCDFMidPointPlugin)

StCDFMidPointPlugin::StCDFMidPointPlugin(double coneRadius,
					 double overlapThreshold,
					 double seedThreshold,
					 double coneAreaFraction)
  : StPlugin(new fastjet::CDFMidPointPlugin(coneRadius,overlapThreshold,seedThreshold,coneAreaFraction))
{
}

StCDFMidPointPlugin::~StCDFMidPointPlugin()
{
  //  delete mImpl;
}
