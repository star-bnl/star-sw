// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: StFastJetPars.h,v 1.4 2016/01/06 22:00:17 gdwebb Exp $
//
// $Log: StFastJetPars.h,v $
// Revision 1.4  2016/01/06 22:00:17  gdwebb
// This is code to implement the off axis cone underlying event analysis.
//
// Revision 1.3  2012/03/10 23:18:28  pibero
// Added destructor for StCDFMidPointPlugin
//
// Revision 1.2  2012/03/10 23:09:53  pibero
// Addeed support for fastjet plugins
//
// Revision 1.1  2011/08/31 17:57:36  pibero
// Support for FastJet
//
// http://www.lpthe.jussieu.fr/~salam/fastjet/
// http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8521.html
//
//

#ifndef ST_FAST_JET_PARS_H
#define ST_FAST_JET_PARS_H

#include "StJetPars.h"
#include "StFastJetAreaPars.h"

class StPlugin : public TObject {
public:
  StPlugin(void* i) : mImpl(i) {}
  virtual ~StPlugin() {}
  void* impl() const { return mImpl; }
protected:
  void* mImpl;
  ClassDef(StPlugin,0)
};

class StCDFMidPointPlugin : public StPlugin {
public:
  StCDFMidPointPlugin(double coneRadius,
		      double overlapTreshold,
		      double seedThreshold,
		      double coneAreaFraction);
  ~StCDFMidPointPlugin();
  ClassDef(StCDFMidPointPlugin,0)
};

class StFastJetPars : public StJetPars {
public:
  /// enum JetAlgorithm:
  ///
  /// the longitudinally invariant kt algorithm
  static const int kt_algorithm;
  /// the longitudinally invariant variant of the cambridge algorithm
  /// (aka Aachen algoithm).
  static const int cambridge_algorithm;
  /// like the k_t but with distance measures 
  ///       dij = min(1/kti^2,1/ktj^2) Delta R_{ij}^2 / R^2
  ///       diB = 1/kti^2
  static const int antikt_algorithm;
  /// like the k_t but with distance measures 
  ///       dij = min(kti^{2p},ktj^{2p}) Delta R_{ij}^2 / R^2
  ///       diB = 1/kti^{2p}
  /// where p = extra_param()
  static const int genkt_algorithm;
  /// a version of cambridge with a special distance measure for particles
  /// whose pt is < extra_param()
  static const int cambridge_for_passive_algorithm;
  /// a version of genkt with a special distance measure for particles
  /// whose pt is < extra_param() [relevant for passive areas when p<=0]
  static const int genkt_for_passive_algorithm;
  //.................................................................
  /// the e+e- kt algorithm
  static const int ee_kt_algorithm;
  /// the e+e- genkt algorithm  (R > 2 and p=1 gives ee_kt)
  static const int ee_genkt_algorithm;
  //.................................................................
  /// any plugin algorithm supplied by the user
  static const int plugin_algorithm;

  /// enum RecombinationScheme:
  ///
  /// summing the 4-momenta
  static const int E_scheme;
  /// pt weighted recombination of y,phi (and summing of pt's)
  /// with preprocessing to make things massless by rescaling E=|\vec p|
  static const int pt_scheme;
  /// pt^2 weighted recombination of y,phi (and summing of pt's)
  /// with preprocessing to make things massless by rescaling E=|\vec p|
  static const int pt2_scheme;
  /// pt weighted recombination of y,phi (and summing of pt's)
  /// with preprocessing to make things massless by rescaling |\vec p|->=E
  static const int Et_scheme;
  /// pt^2 weighted recombination of y,phi (and summing of pt's)
  /// with preprocessing to make things massless by rescaling |\vec p|->=E
  static const int Et2_scheme;
  /// pt weighted recombination of y,phi (and summing of pt's), with 
  /// no preprocessing
  static const int BIpt_scheme;
  /// pt^2 weighted recombination of y,phi (and summing of pt's)
  /// no preprocessing
  static const int BIpt2_scheme;
  /// for the user's external scheme
  static const int external_scheme;

  /// enum Strategy:
  ///
  /// fastest form about 500..10^4
  static const int N2MinHeapTiled;
  /// fastest from about 50..500
  static const int N2Tiled;
  /// legacy
  static const int N2PoorTiled;
  /// fastest below 50
  static const int N2Plain;
  /// worse even than the usual N^3 algorithms
  static const int N3Dumb;
  /// automatic selection of the best (based on N)
  static const int Best;
  /// best of the NlnN variants -- best overall for N>10^4
  static const int NlnN;
  /// legacy N ln N using 3pi coverage of cylinder
  static const int NlnN3pi;
  /// legacy N ln N using 4pi coverage of cylinder
  static const int NlnN4pi;
  /// Chan's closest pair method (in a variant with 4pi coverage),
  /// for use exclusively with the Cambridge algorithm
  static const int NlnNCam4pi;
  static const int NlnNCam2pi2R;
  static const int NlnNCam;
  /// the plugin has been used...
  static const int plugin_strategy;

  StFastJetPars()
    : mJetAlgorithm(kt_algorithm)
    , mRparam(1.0)
    , mRecombScheme(E_scheme)
    , mStrategy(Best)
    , mPtMin(5.0)
    , mPlugin(0)
    , mJetAreaFlag(false)
  {
  }

  StJetFinder* constructJetFinder();

  int    jetAlgorithm       () const { return mJetAlgorithm; }
  double Rparam             () const { return mRparam;       }
  int    recombinationScheme() const { return mRecombScheme; }
  int    strategy           () const { return mStrategy;     }
  double ptMin              () const { return mPtMin;        }
  StFastJetAreaPars* jetArea () const { return mJetArea;      }
  bool jetAreaFlag          () const { return mJetAreaFlag;  }

  void* plugin() const { return mPlugin->impl(); }

  void setJetAlgorithm       (int    jetAlgorithm) { mJetAlgorithm = jetAlgorithm; }
  void setRparam             (double Rparam      ) { mRparam       = Rparam;       }
  void setRecombinationScheme(int    recombScheme) { mRecombScheme = recombScheme; }
  void setStrategy           (int    strategy    ) { mStrategy     = strategy;     }
  void setPtMin              (double ptmin       ) { mPtMin        = ptmin;        }
  void setPlugin(StPlugin* plugin) { mPlugin = plugin; }
  void setJetArea (StFastJetAreaPars* area_pars){ mJetArea = area_pars; mJetAreaFlag = true;};
private:
  int    mJetAlgorithm;
  double mRparam;
  int    mRecombScheme;
  int    mStrategy;
  double mPtMin;
  StPlugin* mPlugin;
  StFastJetAreaPars *mJetArea;
  bool mJetAreaFlag;

  ClassDef(StFastJetPars,0)
};

#endif	// ST_FAST_JET_PARS_H
