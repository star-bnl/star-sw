// $Id: AliHLTTPCCAPerformanceBase.cxx,v 1.11 2010/08/26 15:05:50 ikulakov Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************
#if 1//def DO_TPCCATRACKER_EFF_PERFORMANCE

#include "AliHLTTPCCounters.h"

#include "AliHLTTPCPerformanceBase.h"
#include "AliHLTTPCCAGBHit.h"
#include "AliHLTTPCCAMCTrack.h"
#include "AliHLTTPCCAMCPoint.h"
#include "AliHLTTPCCAOutTrack.h"
#include "AliHLTTPCCAGBTrack.h"
#include "AliHLTTPCCAGBTracker.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCATracklet.h"

#ifndef HLTCA_STANDALONE
#include "AliHLTTPCCADisplay.h"

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"
#endif // HLTCA_STANDALONE


#include <string>
using std::string;

void AliHLTTPCPerformanceBase::SetNewEvent( const AliHLTTPCCAGBTracker * const tracker,
                                AliHLTResizableArray<AliHLTTPCCAHitLabel> *hitLabels,
                                AliHLTResizableArray<AliHLTTPCCAMCTrack> *mcTracks,
                                AliHLTResizableArray<AliHLTTPCCALocalMCPoint> *localMCPoints)
{
  fTracker = tracker;
  
  fHitLabels = hitLabels;
  fMCTracks = mcTracks;
  fLocalMCPoints = localMCPoints;
  
  nMCTracks = (*fMCTracks).Size();

  fEff = AliHLTTPCEfficiencies();
  mcData.resize(0);
  recoData.resize(0);
} // void AliHLTTPCPerformanceBase::SetNewEvent

AliHLTTPCPerformanceBase::AliHLTTPCPerformanceBase():
  fStatNEvents(0),
#ifndef HLTCA_STANDALONE
  NHisto(0),
#endif
  fTracker(0),fHitLabels(0),fMCTracks(0),fLocalMCPoints(0),nRecoTracks(0),nMCTracks(0)
#ifndef HLTCA_STANDALONE
  ,fHistoDir(0)
#endif
{

}

AliHLTTPCPerformanceBase::~AliHLTTPCPerformanceBase()
{
#ifndef HLTCA_STANDALONE
  if (fHistoDir == 0) // don't write in file
    for( int i = 0; i < NHisto; i++ ){
      if (fHistos[i]) delete fHistos[i];
    }
#endif
}

void AliHLTTPCPerformanceBase::Exec( bool PrintFlag )
{
    // Efficiency
  CheckMCTracks();
  MatchTracks();
  EfficiencyPerformance();
  if (PrintFlag) PrintEfficiency();

#ifndef HLTCA_STANDALONE
    // Histos
  FillHistos();

  Draw();
#endif
  
  fStatNEvents++;
} // Exec


void AliHLTTPCPerformanceBase::EfficiencyPerformance() // TODO add common parts of code
{
  fEff.IncNEvents();
  fEffStat += fEff;
}

#ifndef HLTCA_STANDALONE
TH1 *AliHLTTPCPerformanceBase::GetHisto( int iHisto )
{
  
//   assert ( (iHisto != NHisto) || (string("") == string(" wrong histo name ")) );
  if ( (iHisto >= NHisto) || (iHisto<0) ){
    cout << "ERROR: wrong histo number: " << iHisto << endl;
    exit(1);
  }
  
  return fHistos[iHisto];
}
#endif

#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

