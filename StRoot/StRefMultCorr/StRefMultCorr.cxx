// C++ headers
#include <algorithm>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <limits>

// StRefMultCorr headers
#include "StRefMultCorr.h"
#include "Param.h"

// ROOT headers
#include "TError.h"
#include "TRandom.h"
#include "TMath.h"

ClassImp(StRefMultCorr)

namespace {
  typedef std::pair<Double_t, Int_t> keys;
}

//_________________
// Default constructor
StRefMultCorr::StRefMultCorr(const TString name, const TString subname,
                             const TString libname) : 
                             mName(name), mSubName(subname), mLibName(libname),
                             mRefX(1), mVerbose(kFALSE) {
  mRefMult = 0 ;
  mVz = -9999. ;
  mRefMult_corr = -1.0;
  mIsRu = false;
  mIsZr = false;

  std::cout << mSubName.Data() <<"  "<< mLibName.Data() << std::endl;

  // Clear all data members
  clear() ;

  readHeaderFile() ;
  readBadRunsFromHeaderFile() ;
}

//_________________
// Default destructor
StRefMultCorr::~StRefMultCorr() {
  /* empty */
}

//_________________
Int_t StRefMultCorr::getBeginRun(const Double_t energy, const Int_t year) {
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  std::multimap<keys, Int_t>::iterator iterCheck = mBeginRun.find(key);
  if ( iterCheck == mBeginRun.end() ) {
    Error("StRefMultCorr::getBeginRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  std::pair<std::multimap<keys, Int_t>::iterator, std::multimap<keys, Int_t>::iterator> iterRange = mBeginRun.equal_range(key);

  return (*(iterRange.first)).second ;
}

//________________
Int_t StRefMultCorr::getEndRun(const Double_t energy, const Int_t year) {
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  std::multimap<keys, Int_t>::iterator iterCheck = mEndRun.find(key);
  if (iterCheck == mEndRun.end()) {
    Error("StRefMultCorr::getEndRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  std::pair<std::multimap<keys, Int_t>::iterator, std::multimap<keys, Int_t>::iterator> iterRange = mEndRun.equal_range(key);
  std::multimap<keys, Int_t>::iterator iter = iterRange.second;
  iter--;

  return (*iter).second;
}

//_________________
void StRefMultCorr::clear() {
  // Clear all arrays, and set parameter index = -1

  mYear.clear() ;
  mStart_runId.clear() ;
  mStop_runId.clear() ;
  mStart_zvertex.clear() ;
  mStop_zvertex.clear() ;
  mNormalize_stop.clear() ;

  for(Int_t i=0; i<mNCentrality; i++) {
    mCentrality_bins[i].clear() ;
  }
	
  mParameterIndex = -1 ;

  for(Int_t i=0;i<mNPar_z_vertex;i++) {
    mPar_z_vertex[i].clear() ;
  }

  for(Int_t i=0;i<mNPar_weight;i++) {
    mPar_weight[i].clear();
  }

  for(Int_t i=0;i<mNPar_luminosity;i++) {
    mPar_luminosity[i].clear();
  }

  mBeginRun.clear() ;
  mEndRun.clear() ;
  mBadRun.clear() ;

  mnVzBinForWeight = 0 ;
  mVzEdgeForWeight.clear();
  mgRefMultTriggerCorrDiffVzScaleRatio.clear() ;
}

//_________________
Bool_t StRefMultCorr::isBadRun(const Int_t RunId) {
  // Return true if a given run id is bad run
  std::vector<Int_t>::iterator iter = std::find(mBadRun.begin(), mBadRun.end(), RunId);
#if 0
  if ( iter != mBadRun.end() ) {
    // QA
    std::cout << "StRefMultCorr::isBadRun  Find bad run = " << (*iter) << std::endl;
  }
#endif

  return ( iter != mBadRun.end() ) ;
}

//_________________
void StRefMultCorr::initEvent(const UShort_t RefMult, const Double_t z, 
                              const Double_t zdcCoincidenceRate) {
  if (mVerbose) {
    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
    std::cout << "+++++++++++++++++++++           New Event                 +++++++++++++++++++\n";
    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n";
  }
  // Set refmult, vz and corrected refmult if current (refmult,vz) are different from inputs
  // User must call this function event-by-event before 
  // calling any other public functions
  if ( mRefMult != RefMult || mVz != z || mZdcCoincidenceRate != zdcCoincidenceRate ) {
    mRefMult            = RefMult ;
    mVz                 = z ;
    mZdcCoincidenceRate = zdcCoincidenceRate ;
    mRefMult_corr       = getRefMultCorr(mRefMult, mVz, mZdcCoincidenceRate) ;
  }
}

//_________________
Bool_t StRefMultCorr::passnTofMatchRefmultCut(Double_t refmult, Double_t ntofmatch, 
                                              Double_t vz /* default = 0*/) const {


  if (mVerbose) {
    std::cout << "Pile up check...";
  }
  Double_t a0{}, a1{}, a2{}, a3{}, a4{};
  Double_t b0{}, b1{}, b2{}, b3{}, b4{};
  Double_t c0{}, c1{}, c2{}, c3{}, c4{};
  Double_t refmultcutmax{};
  Double_t refmultcutmin{};

  Bool_t notPileUp = kFALSE;

  // TODO:
  // Reference multiplicity dependent pile up rejection parameter selection
  // Be aware that the parameters are hardcoded (should be replaced with the
  // arrays stored in Param.h)

  if (mRefX == 1) { // refMult
    if( mParameterIndex>=30 && mParameterIndex<=35 ) { // Au+Au 27 GeV 2018
      const Double_t min = 4.0;
      const Double_t max = 5.0;

      if(ntofmatch<=2)  return false;

      a0 = -0.704787625248525;
      a1 = 0.99026234637141;
      a2 = -0.000680713101607504;
      a3 = 2.77035215460421e-06;
      a4 = -4.04096185674966e-09;
      b0 = 2.52126730672253;
      b1 = 0.128066911940844;
      b2 = -0.000538959206681944;
      b3 = 1.21531743671716e-06;
      b4 = -1.01886685404478e-09;
      c0 = 4.79427731664144;
      c1 = 0.187601372159186;
      c2 = -0.000849856673886957;
      c3 = 1.9359155975421e-06;
      c4 = -1.61214724626684e-09;

      double refmultCenter = a0+a1*(ntofmatch)+a2*pow(ntofmatch,2)+a3*pow(ntofmatch,3)+a4*pow(ntofmatch,4);
      double refmultLower  = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      double refmultUpper  = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);

      refmultcutmin = refmultCenter - min*refmultLower;
      refmultcutmax = refmultCenter + max*refmultUpper;
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobars 200 GeV 2018

      // Zr+Zr
      if(mParameterIndex==36) {
        b0=13.5244327901538;
        b1=1.4429201808933;
        b2=-0.002873496957537;
        b3=7.29172798142226e-06;
        b4=-7.45759942317285e-09;
        c0=-11.2781454979572;
        c1=0.420728494449501;
        c2=0.00184005031913895;
        c3=-4.61008765754698e-06;
        c4=4.28291905929182e-09;
      }
      // Ru+Ru
      else if(mParameterIndex==37) {
        b0=13.5426221755897;
        b1=1.44261201539344;
        b2=-0.00288428931227279;
        b3=7.35384541646783e-06;
        b4=-7.53407759526067e-09;
        c0=-11.2591376113937;
        c1=0.419541462167548;
        c2=0.00185578651291454;
        c3=-4.68933832723005e-06;
        c4=4.4151761900593e-09;
      }
      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==38 ) { // Au+Au 19.6 GeV 2019

      if ( -145. <= vz && vz < -87. ) {
        b0=33.7732676854599;
        b1=1.75937881368933;
        b2=-0.00285868075552296;
        b3=8.50344260510873e-06;
        b4=-1.19215380537174e-08;
        c0=-22.7060232139884;
        c1=0.863809402986806;
        c2=-0.000368119767293671;
        c3=5.97122714011036e-06;
        c4=-1.27438638584224e-08;
      }
      else if ( -87. <= vz && vz < -29. ) {
        b0=20.5875336291458;
        b1=1.67371122493626;
        b2=-0.00307534477962496;
        b3=7.93755518827246e-06;
        b4=-8.46257293600085e-09;
        c0=-15.5923736275086;
        c1=0.604206551537668;
        c2=0.00131805594121643;
        c3=-2.04753779335401e-06;
        c4=5.73181898751325e-10;
      }
      else if ( -29. <= vz && vz < 29. ) {
        b0=15.1015102672534;
        b1=1.53929151189229;
        b2=-0.00269203062814483;
        b3=6.488759952638e-06;
        b4=-6.06073586314757e-09;
        c0=-13.1157864223955;
        c1=0.504707692972168;
        c2=0.00187997948645203;
        c3=-4.7317012773039e-06;
        c4=4.8234194091071e-09;
      }
      else if ( 29. <= vz && vz < 87. ) {
        b0=20.7718852504153;
        b1=1.67316129891511;
        b2=-0.00315093393202473;
        b3=8.35823870487966e-06;
        b4=-9.14822467807924e-09;
        c0=-15.9411138444366;
        c1=0.61506063963685;
        c2=0.0011824174541949;
        c3=-1.48902496972716e-06;
        c4=-2.29371463231934e-10;
      }
      else if ( 87. <= vz && vz <= 145. ) {
        b0=33.4926150575549;
        b1=1.79372677959986;
        b2=-0.00319461487211403;
        b3=9.56612691680003e-06;
        b4=-1.31049413530369e-08;;
        c0=-22.4679773305418;
        c1=0.83906220918966;
        c2=-0.000106213494253586;
        c3=4.93946486222714e-06;
        c4=-1.1450298089717e-08;
      }

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==39 ) { // Au+Au 14.6 GeV 2019
        b0 =  36.4811873257854;
        b1 =  1.96363692967013;
        b2 = -0.00491528146300182;
        b3 =  1.45179464078414e-05;;
        b4 = -1.82634741809226e-08;
        c0 = -16.176117733536;;
        c1 =  0.780745107634961;
        c2 = -2.03347057620351e-05;
        c3 =  3.80646723724747e-06;
        c4 = -9.43403282145648e-09;
        refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
        refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
        notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==40 ) { // Au+Au 200 GeV 2019

      b0=18.0459;
      b1=1.32913;
      b2=-0.000929385;
      b3=1.53176e-06;
      b4=-9.911e-10;
      c0=-18.7481;
      c1=0.785467;
      c2=2.12757e-05;
      c3=3.4805e-07;
      c4=-3.80776e-10;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==41 ) { // Au+Au 7.7 GeV 2020

      if ( -145. <= vz && vz < -87. ) {
        b0=39.578630496797;
        b1=1.46561577132993;
        b2=0.006515367058115;
        b3=-4.06391982010589e-05;
        b4=5.51203917383809e-08;
        c0=-14.8817460248614;
        c1=0.764539480062978;
        c2=0.00368901349656326;
        c3=-1.27602217700865e-05;
        c4=8.02618485000158e-10;
      }
      else if ( -87. <= vz && vz < -29. ) {
        b0=26.1841414192908;
        b1=1.73354655107464;
        b2=-0.00280668326418846;
        b3=1.22370803379957e-05;
        b4=-3.15068617200212e-08;
        c0=-13.1831127837376;
        c1=0.760227210117286;
        c2=0.00195873375843822;
        c3=-2.69378951644624e-06;
        c4=-1.05344843941749e-08;
      }
      else if ( -29. <= vz && vz < 29. ) {
        b0=23.3635904884101;
        b1=1.58179764458174;
        b2=-0.00100184372825271;
        b3=7.76378744751984e-07;
        b4=-6.46469867000365e-09;
        c0=-11.4340781454132;
        c1=0.72398407747444;
        c2=0.00121092416745035;
        c3=1.17875404059176e-07;
        c4=-9.81658682040738e-09;
      }
      else if ( 29. <= vz && vz < 87. ) {
        b0=29.4343991835005;
        b1=1.48353715105631;
        b2=0.00106271734149745;
        b3=-9.07835076338586e-06;
        b4=6.7722581625238e-09;
        c0=-9.97159163811459;
        c1=0.591000613390771;
        c2=0.00449768928484714;
        c3=-1.71667412152202e-05;
        c4=1.6467383813372e-08;
      }
      else if ( 87. <= vz && vz <= 145. ) {
        b0=37.0772875081557;
        b1=1.53484162926915;
        b2=0.00471873506675937;
        b3=-2.94958548877277e-05;
        b4=3.60887574265838e-08;
        c0=-13.3927733032856;
        c1=0.704319390196747;
        c2=0.00485360248820988;
        c3=-2.10416804123978e-05;
        c4=1.92342533435503e-08;
      }

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==42 ) { // Au+Au 9.2 GeV 2020 TriggerID = 780020

      if ( -145. <= vz && vz < -87. ) {
        b0=25.6055790979197;
        b1=2.02528136596901;
        b2=-0.0058370984051939;
        b3=2.59602314466234e-05;
        b4=-5.3014743584261e-08;
        c0=-17.7059596791057;
        c1=0.614538168662738;
        c2=0.00534180935164814;
        c3=-1.79582873880806e-05;
        c4=1.01623054170579e-08;
      }
      else if ( -87. <= vz && vz < -29. ) {
        b0=23.0160060308621;
        b1=1.61885832757588;
        b2=-0.00275873189631398;
        b3=1.31262550392554e-05;
        b4=-2.94368020941846e-08;
        c0=-17.3591842617911;
        c1=0.796170989774258;
        c2=0.000670722514533827;
        c3=3.26258075150876e-06;
        c4=-1.60611460182112e-08;
      }
      else if ( -29. <= vz && vz < 29. ) {
        b0=16.4277056306649;
        b1=1.71652229539398;
        b2=-0.00406847684302521;
        b3=1.65203560938885e-05;
        b4=-2.96250329214512e-08;
        c0=-15.7887025834219;
        c1=0.789786364309292;
        c2=-0.000637115144252616;
        c3=1.00019972792727e-05;
        c4=-2.45208851616324e-08;        
      }
      else if ( 29. <= vz && vz < 87. ) {
        b0=21.2024767158778;
        b1=1.70521848381614;
        b2=-0.00352260930859763;
        b3=1.60905730948817e-05;
        b4=-3.37443468806432e-08;
        c0=-17.1166088395929;
        c1=0.814739436616432;
        c2=0.000227197779215977;
        c3=6.55397838050604e-06;
        c4=-2.28812912596058e-08;
      }
      else if ( 87. <= vz && vz <= 145. ) {
        b0=26.0970905882739;
        b1=1.88889714311734;
        b2=-0.00195374948885512;
        b3=-6.14244087431038e-06;
        b4=1.99930095058841e-08;
        c0=-15.6624325989392;
        c1=0.52385751891358;
        c2=0.00794996911844969;
        c3=-4.09239155250494e-05;
        c4=6.40163739983216e-08;
      }

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==43 ) { // Au+Au 17.3 GeV 2021

      if ( -145. <= vz && vz < -87. ) {
        b0=25.8023785946209;
        b1=1.80974818833103;
        b2=-0.00230107205687879;
        b3=1.04069753338853e-05;
        b4=-2.43265995270951e-08;
        c0=-25.7628397848;
        c1=1.15844463977968;
        c2=-0.00285234327923795;
        c3=1.68279361312683e-05;
        c4=-2.89872992178789e-08;
      }
      else if ( -87. <= vz && vz < -29. ) {
        b0=26.2142811336132;
        b1=1.40180659301151;
        b2=-0.000197781802002694;
        b3=1.02666189094347e-06;
        b4=-5.52762010064236e-09;
        c0=-21.4352021999217;
        c1=1.01067273031472;
        c2=-0.00160328567162831;
        c3=8.94486444751978e-06;
        c4=-1.46093145276812e-08;
      }
      else if ( -29. <= vz && vz < 29. ) {
        b0=20.1361585417616;
        b1=1.54339163322734;
        b2=-0.00277257992675217;
        b3=1.01670412308599e-05;
        b4=-1.4564482074994e-08;
        c0=-18.0093218064881;
        c1=0.858263071231256;
        c2=-0.000411359635522234;
        c3=4.21562873026016e-06;
        c4=-8.07993954642765e-09;
      }
      else if ( 29. <= vz && vz < 87. ) {
        b0=25.8570023358432;
        b1=1.37245590215625;
        b2=-5.45184310087876e-05;
        b3=6.25643605701836e-07;
        b4=-4.90542835006027e-09;
        c0=-20.7158089395719;
        c1=1.00148007639466;
        c2=-0.00138806953636318;
        c3=7.92595642206008e-06;
        c4=-1.32107375325913e-08;
      }
      else if ( 87. <= vz && vz <= 145. ) {
        b0=28.2036847494035;
        b1=1.640750436652;
        b2=-0.000569887807630565;
        b3=3.95821109316978e-06;
        b4=-1.60367555403757e-08;
        c0=-26.3129222166004;
        c1=1.21481523017369;
        c2=-0.00341644731702994;
        c3=1.84782571448044e-05;
        c4=-3.03333077890128e-08;
      }

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if ( mParameterIndex==44 ) { // Au+Au 11.5 GeV 2020

      if ( -145. <= vz && vz < -87. ) {
        b0=18.0402708948567;
        b1=2.09478604674414;
        b2=-0.00685576746251115;
        b3=3.88333589216404e-05;
        b4=-8.12179090437804e-08;
        c0=-12.7515169659501;
        c1=0.705235205311516;
        c2=0.00321598985910965;
        c3=-1.56896265545575e-05;
        c4=2.97072869656044e-08;
      }
      else if ( -87. <= vz && vz < -29. ) {
        b0=14.2601983060724;
        b1=1.71255613728895;
        b2=-0.00383919825526746;
        b3=1.7756145374654e-05;
        b4=-3.19509246865534e-08;
        c0=-10.9408282877465;
        c1=0.617024824873745;
        c2=0.00264576299008488;
        c3=-1.158420066816e-05;
        c4=2.01763088491799e-08;
      }
      else if ( -29. <= vz && vz < 29. ) {
        b0=11.1331231719184;
        b1=1.69710478538775;
        b2=-0.00464826171041643;
        b3=2.02639545153783e-05;
        b4=-3.4169236655577e-08;
        c0=-8.82209022882564;
        c1=0.524312884632579;
        c2=0.00321682247003759;
        c3=-1.35894996081641e-05;
        c4=2.26005417512409e-08;
      }
      else if ( 29. <= vz && vz < 87. ) {
        b0=14.615141872526;
        b1=1.69217111894767;
        b2=-0.00377600546419821;
        b3=1.83551619792816e-05;
        b4=-3.48332786210067e-08;
        c0=-11.0113966446419;
        c1=0.616128886729022;
        c2=0.00278642638292705;
        c3=-1.3124493295967e-05;
        c4=2.44388293439677e-08;
      }
      else if ( 87. <= vz && vz <= 145. ) {
        b0=17.988224598148;
        b1=2.07853473508418;
        b2=-0.00668791264313384;
        b3=3.61562317906595e-05;
        b4=-7.30405696800251e-08;
        c0=-12.6730707166176;
        c1=0.709713827776669;
        c2=0.00318794623382361;
        c3=-1.47530903374243e-05;
        c4=2.55638251982488e-08;
      }

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else {
      notPileUp = kTRUE;
    }

  } // if (mRefX == 1) { // refMult
  else if ( mRefX == 5 ) { // fxtMult
    if (mParameterIndex == 1) { // Run 19 Au+Au 4.59 GeV (sqrt(s_NN)=3.2 GeV)
      b0=19.48;
      b1=5.428;
      b2=-0.007;
      b3=-2.428e-4;
      b4=1.197e-7;
      c0=-13.59;
      c1=1.515;
      c2=0.02816;
      c3=-1.195e-4;
      c4=-9.639e-7;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    }
    else if (mParameterIndex == 2) { // Run 20 Au+Au 5.75 GeV (sqrt(s_NN)=3.5 GeV)
      b0=23.28;
      b1=5.247;
      b2=0.04037;
      b3=-1.206e-3;
      b4=5.792e-6;
      c0=-14.82;
      c1=1.583;
      c2=0.02684;
      c3=4.605e-5;
      c4=-2.410e-6;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 3) { // Run 19 Au+Au 7.3 GeV (sqrt(s_NN)=3.9 GeV)
      b0=31.5858;
      b1=4.29274;
      b2=0.0485779;
      b3=-1.039e-3;
      b4=4.408e-6;
      c0=-22.7905;
      c1=2.6578;
      c2=-0.03112;
      c3=1.031e-3;
      c4=-7.434e-6;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 4) { // Run 20 Au+Au 7.3 GeV (sqrt(s_NN)=3.9 GeV)
      b0=29.74;
      b1=4.421;
      b2=0.09139;
      b3=-1.977e-3;
      b4=9.435e-6;
      c0=-20.53;
      c1=2.557;
      c2=-0.02094;
      c3=8.943e-4;
      c4=-6.879e-6;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 5) { // Run 20 Au+Au 31.2 GeV (sqrt(s_NN)=7.7 GeV)
      b0=24.7299053323955;
      b1=7.79546460550082;
      b2=-0.000336278299464254;
      b3=-0.000549204114892259;
      b4=1.89274000668251e-06;
      c0=-24.3335976220474;
      c1=1.93207052914575;
      c2=0.0042539477677528;
      c3=0.000725893349545147;
      c4=-6.29726910263091e-06;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 6) { // Run 20 Au+Au 5.75 GeV (sqrt(s_NN)=3.5 GeV)
      b0=23.28;
      b1=5.247;
      b2=0.04037;
      b3=-1.206e-3;
      b4=5.792e-6;
      c0=-14.82;
      c1=1.583;
      c2=0.02684;
      c3=4.605e-5;
      c4=-2.410e-6;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 7) { // Run 20 Au+Au 13.5 GeV (sqrt(s_NN)=5.2 GeV)
      b0=18.6707;
      b1=6.92307;
      b2=-0.0293523;
      b3=0.000412261;
      b4=-4.74922e-06;      
      c0=-14.4436;
      c1=-0.047413;
      c2=0.100793;
      c3=-0.00121203;
      c4=5.59521e-06;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }    
    else if (mParameterIndex == 8) { // Run 20 Au+Au 19.5 GeV (sqrt(s_NN)=6.2 GeV)
      b0=25.0191;
      b1=5.51924;
      b2=0.0694824;
      b3=-0.00121388;
      b4=3.44057e-06;
      c0=-16.9132;
      c1=2.35278;
      c2=-0.0341491;
      c3=0.00131257;
      c4=-9.0295e-06;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 9) { // Run 20 Au+Au 9.8 GeV (sqrt(s_NN)=4.5 GeV) TriggerID = 740000
      b0=24.7124572851806;
      b1=5.71868597816542;
      b2=-0.00312345552143256;
      b3=-0.00017256714660496;
      b4=-6.34006022264486e-07;
      c0=-32.7658746560952;
      c1=2.70684190541053;
      c2=0.00128464121679533;
      c3=0.000470785124435377;
      c4=-4.47307728635742e-06;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);  
    }
    else if (mParameterIndex == 10) { // Run 20 Au+Au 9.8 GeV (sqrt(s_NN)=4.5 GeV) TriggerID = 740010
      b0=35.02;
      b1=3.586;
      b2=0.1368;
      b3=-2.578e-3;
      b4=1.189e-5;
      c0=-24.84;
      c1=3.289;
      c2=-0.05722;
      c3=1.491e-3;
      c4=-9.678e-6;

      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
      if (ntofmatch >= 95 && refmult > 367) notPileUp = kFALSE;
    }
    else {
      notPileUp = kTRUE;
    }
  } // else if ( mRefX == 5 ) { // fxtMult
  else if ( mRefX == 6 ) { // refMult6
    if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
      b0 = 10.805229670974072;
      b1 = 4.222122255649001;
      b2 = -0.059023457533258925;
      b3 = 0.0007297279166792341;
      b4 = -3.730248246986408e-06;
      c0 = -22.14994423125864;
      c1 = 2.295827717755825;
      c2 = -0.007304309390513328;
      c3 = 9.397986288422607e-05;
      c4 = -9.080150075680224e-07;
      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    } // if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
    else if ( mParameterIndex==1 ) { // d+Au 200 GeV 2021
      b0 =  2.4412914662443033;
      b1 =  5.523540420923605;
      b2 = -0.16458436958697667;
      b3 =  0.002805908341435613;
      b4 = -1.6300934820294975e-05;
      c0 = -0.86595124167792;
      c1 =  0.44263208748354943;
      c2 =  0.06024976895762696;
      c3 = -0.0013523620327006189;
      c4 =  1.0553696607739253e-05;
      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    } // else if ( mParameterIndex==1 ) { // d+Au 200 GeV 2021
    else {
      notPileUp = kTRUE;
    }  
  } // else if ( mRefX == 6 ) { // refMult6
  else if ( mRefX == 7 ) { // TotnMIP
    if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
      b0 = 10.805229670974072;
      b1 = 4.222122255649001;
      b2 = -0.059023457533258925;
      b3 = 0.0007297279166792341;
      b4 = -3.730248246986408e-06;
      c0 = -22.14994423125864;
      c1 = 2.295827717755825;
      c2 = -0.007304309390513328;
      c3 = 9.397986288422607e-05;
      c4 = -9.080150075680224e-07;
      refmultcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      refmultcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(refmult, refmultcutmin, refmultcutmax);
    } // if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
    else {
      notPileUp = kTRUE;
    }  
  } // else if ( mRefX == 7 ) { // TotnMIP


  if (mVerbose) {
    std::cout << "\t notPileUp: ";
    if (notPileUp) {
      std::cout << "TRUE";
    } 
    else {
      std::cout << "FALSE";
    }
    std::cout << "\t[DONE]\n";
  }

  return notPileUp;
}

//_________________
Bool_t StRefMultCorr::passnTofMatchTotnMIPCut(Double_t totnMIP, Double_t ntofmatch, 
                                              Double_t vz /* default = 0*/) const {


  if (mVerbose) {
    std::cout << "Pile up check using nBTOFMatch vs. totnMIP...";
  }
  Double_t b0{}, b1{}, b2{}, b3{}, b4{};
  Double_t c0{}, c1{}, c2{}, c3{}, c4{};
  Double_t totnMIPcutmax{};
  Double_t totnMIPcutmin{};

  Bool_t notPileUp = kFALSE;

  if ( mRefX == 6 ) { // refMult6
    if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
      b0 = 49.92964577941192;
      b1 = 30.084027095279428;
      b2 = -0.6414804471204509;
      b3 = 0.006675174653594674;
      b4 = -2.690799009087484e-05;
      c0 = -114.51733250850462;
      c1 = 2.2552394816896664;
      c2 = 0.17302732482464722;
      c3 = -0.002572787709722221;
      c4 = 9.276963258195168e-06;
      totnMIPcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      totnMIPcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(totnMIP, totnMIPcutmin, totnMIPcutmax);
    } // if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
    else if ( mParameterIndex==1 ) { // d+Au 200 GeV 2021
      b0 = 13.854308990154239;
      b1 = 48.38889240643103;
      b2 = -1.820427172052759;
      b3 = 0.0313802142314363;
      b4 = -0.00018680122215220316;
      c0 = 7.022386992461804;
      c1 = -3.747248801082249;
      c2 = 0.36559511738864753;
      c3 = -0.00836993562719528;
      c4 = 7.050862326468557e-05;
      totnMIPcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      totnMIPcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(totnMIP, totnMIPcutmin, totnMIPcutmax);
    } // else if ( mParameterIndex==1 ) { // d+Au 200 GeV 2021
    else {
      notPileUp = kTRUE;
    }  
  } // if ( mRefX == 6 ) { // refMult6
  else if ( mRefX == 7 ) { // TotnMIP
    if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
      b0 = 49.92964577941192;
      b1 = 30.084027095279428;
      b2 = -0.6414804471204509;
      b3 = 0.006675174653594674;
      b4 = -2.690799009087484e-05;
      c0 = -114.51733250850462;
      c1 = 2.2552394816896664;
      c2 = 0.17302732482464722;
      c3 = -0.002572787709722221;
      c4 = 9.276963258195168e-06;
      totnMIPcutmax = calcPileUpRefMult(ntofmatch, b0, b1, b2, b3, b4);
      totnMIPcutmin = calcPileUpRefMult(ntofmatch, c0, c1, c2, c3, c4);
      notPileUp = isInPileUpRefMultLimits(totnMIP, totnMIPcutmin, totnMIPcutmax);
    } // if ( mParameterIndex==0 ) { // O+O 200 GeV 2021
    else {
      notPileUp = kTRUE;
    }  
  } // else if ( mRefX == 7 ) { // TotnMIP

  if (mVerbose) {
    std::cout << "\t notPileUp using : ";
    if (notPileUp) {
      std::cout << "TRUE";
    } 
    else {
      std::cout << "FALSE";
    }
    std::cout << "\t[DONE]\n";
  }

  return notPileUp;
}

//_________________
Bool_t StRefMultCorr::isIndexOk() const {
  // mParameterIndex not initialized (-1)
  if ( mParameterIndex == -1 ) {
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. Call init(const Int_t RunId) function to initialize centrality bins, corrections");
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. or use valid run numbers defined in Centrality_def_%s.txt", mName.Data());
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. exit");
    std::cout << std::endl;
    // Stop the process if invalid run number found
    // exit(0);
    return kFALSE;
  }

  // Out of bounds
  if ( mParameterIndex >= (Int_t)mStart_runId.size() ) {
    Error("StRefMultCorr::isIndexOk",
	  Form("mParameterIndex = %d > max number of parameter set = %d. Make sure you put correct index for this energy",
	       mParameterIndex, mStart_runId.size()));
    return kFALSE ;
  }

  return kTRUE ;
}

//_________________
Bool_t StRefMultCorr::isZvertexOk() const {
  // Primary z-vertex check
  return (mVz > mStart_zvertex[mParameterIndex] &&
          mVz < mStop_zvertex[mParameterIndex]);
}

//_________________
Bool_t StRefMultCorr::isRefMultOk() const {
  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // select 0-80%
  return (mRefMult_corr > mCentrality_bins[0][mParameterIndex] &&
          mRefMult_corr < mCentrality_bins[mNCentrality][mParameterIndex]);
}

//_________________
Bool_t StRefMultCorr::isCentralityOk(const Int_t icent) const {
  // Invalid centrality id
  if ( icent < -1 || icent >= mNCentrality+1 ) return kFALSE ;

  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // Special case
  // 1. 80-100% for icent=-1
  if ( icent == -1 ) return ( mRefMult_corr <= mCentrality_bins[0][mParameterIndex] );

  // 2. icent = mNCentrality
  if ( icent == mNCentrality ) return ( mRefMult_corr <= mCentrality_bins[mNCentrality][mParameterIndex] );

  const Bool_t isOK = ( mRefMult_corr > mCentrality_bins[icent][mParameterIndex] && 
			mRefMult_corr <= mCentrality_bins[icent+1][mParameterIndex] );
  if(mVerbose && isOK) {
    std::cout << "StRefMultCorr::isCentralityOk  refmultcorr = " << mRefMult_corr
              << "  min. bin = " << mCentrality_bins[icent][mParameterIndex]
              << "  max. bin = " << mCentrality_bins[icent+1][mParameterIndex]
              << std::endl;
  }
  return isOK ;
}

//_________________
void StRefMultCorr::init(const Int_t RunId) {
  // Reset mParameterIndex
  mParameterIndex = -1 ;

  // call setParameterIndex
  setParameterIndex(RunId) ;
}

//_________________
Int_t StRefMultCorr::setParameterIndex(const Int_t RunId) {
  // Determine the corresponding parameter set for the input RunId
  for(UInt_t npar = 0; npar < mStart_runId.size(); npar++) {
    if(RunId >= mStart_runId[npar] && RunId <= mStop_runId[npar]) {
      mParameterIndex = npar ;
      //      std::cout << "StRefMultCorr::setParameterIndex  Parameter set = " << mParameterIndex << " for RUN " << RunId << std::endl;
      break ;
    }
  }
	
  // Multiple parameters/definitions for Run14/16 data
  // Set mParameterIndex by hand
  // For Run14 P16id production
  // For Run16 P16ij production
  if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) { 
    if ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ) {
      if(RunId/1000000==15 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 ) {
        mParameterIndex = 0;
        if (mVzEdgeForWeight.empty()) {
          setVzForWeight(nWeightVzBin_Run14_P16id,
                         WeightVzEdgeMin_Run14_P16id,
                         WeightVzEdgeMax_Run14_P16id);
        }
        if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
          readScaleForWeight(nWeightgRefmultBin_Run14_P16id,
                             weight_VpdMB5ToVpdMB30_Run14_P16id);
        }
      } // if(RunId/1000000==15 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
    } // if ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 )
    else if ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0) {
        // prod.1
        if (RunId / 1000 >= 17039 && RunId / 1000 <= 17130) {
          mParameterIndex = 4;
          if (mVzEdgeForWeight.empty()) {
            setVzForWeight(nWeightVzBin_Run16_P16ij_prod1,
                           WeightVzEdgeMin_Run16_P16ij_prod1,
                           WeightVzEdgeMax_Run16_P16ij_prod1);
          }
          if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
            readScaleForWeight(nWeightgRefmultBin_Run16_P16ij_prod1,
                               weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod1);
          }
        } // if (RunId / 1000 >= 17039 && RunId / 1000 <= 17130)
        // prod.2
        else if (RunId / 1000 >= 17169 && RunId / 1000 <= 17179) {
          mParameterIndex = 5;
          if (mVzEdgeForWeight.empty()) {
            setVzForWeight(nWeightVzBin_Run16_P16ij_prod2,
                           WeightVzEdgeMin_Run16_P16ij_prod2,
                           WeightVzEdgeMax_Run16_P16ij_prod2);
          }
          if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
            readScaleForWeight(nWeightgRefmultBin_Run16_P16ij_prod2,
                               weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod2);
          }
        } // else if (RunId / 1000 >= 17169 && RunId / 1000 <= 17179)
      } // if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0)
    } // else if ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 )
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 ) {
      if (mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0) {
        mParameterIndex = 1;
      }
    }
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0) {
        mParameterIndex = 2;
      }
    }
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0) {
        mParameterIndex = 3;
      }
    }
    else if ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0) {
        mParameterIndex = 6;
      }
    }
    else{
      mParameterIndex = -1;
    }
  }	

  // Run numbers for isobar data are not successive
  if ( mName.CompareTo("refmult", TString::kIgnoreCase) == 0 &&
       mSubName.CompareTo("Isobar", TString::kIgnoreCase) == 0 ) {

    //	std::cout <<"This is isobaric runs"<< std::endl;
    mIsZr = mIsRu = kFALSE;
    for (Int_t i = 0; i < nRunId_Zr; i++) {
      if (RunId == IsobarRunId_Zr[i]) {
        mIsZr = kTRUE;
        mParameterIndex = 36;
        //	std::cout <<"--> Zr+Zr"<< std::endl;
        break;
      }
    }
    for (Int_t i = 0; i < nRunId_Ru; i++) {
      if (RunId == IsobarRunId_Ru[i]) {
        mIsRu = kTRUE;
        mParameterIndex = 37;
        //	std::cout <<"--> Ru+Ru"<< std::endl;
        break;
      }
    }
    if(!mIsZr && !mIsRu) {
      Error("StRefMultCorr::setParameterIndex", "RUN %d is not isobaric data", RunId);
    }
  }

  //	std::cout <<"mParameterIndex = "<< mParameterIndex << std::endl;

  if(mParameterIndex == -1) {
    Error("StRefMultCorr::setParameterIndex", "Parameter set does not exist for RUN %d", RunId);
  }
  //else std::cout << "Parameter set = " << npar_set << std::endl;

  if (mVerbose) {
    std::cout << "Parameter index set to: " << mParameterIndex << std::endl; 
  }

  return mParameterIndex ;
}

//_________________
Double_t StRefMultCorr::getRefMultCorr() const {
  // Call initEvent() first
  return mRefMult_corr ;
}

//________________
Double_t StRefMultCorr::luminosityCorrection(Double_t zdcCoincidenceRate) const {

  Double_t lumiCorr = 1.;
  
  if ( mVerbose ) {
    std::cout << "Estimation of luminosity correction factor..." << std::endl;
    std::cout << "\t ZDC coincidence rate: " << zdcCoincidenceRate << " BBC coincidence rate: " << 0 << std::endl;
  }
  // 200 GeV only. correction = 1 for all the other energies for BES-I
  // the above statement may not true for BES-II, since the luminosity is much higher than BES-I, add by Zaochen
  // better to check the <Refmult> vs ZDCX to see whether they are flat or not, add by Zaochen
  const Double_t par0_lum = mPar_luminosity[0][mParameterIndex] ;
  const Double_t par1_lum = mPar_luminosity[1][mParameterIndex] ;
  
  if( mParameterIndex==36 || mParameterIndex==37 || mParameterIndex==40 || 
      ( mRefX==5 && mParameterIndex==7 ) || 
      ( mRefX==5 && mParameterIndex==8 ) ) {
    // if(mYear[mParameterIndex] == 2018 && mIsZr) zdcmean = 96.9914;
    // if(mYear[mParameterIndex] == 2018 && mIsRu) zdcmean = 97.9927;
    Double_t b_prime = 1.;
    if(mParameterIndex==36) b_prime = 96.9914; // Zr
    if(mParameterIndex==37) b_prime = 97.9927; // Ru
    if(mParameterIndex==40) b_prime = 213.383; // AuAu 200GeV Run19
    if(mParameterIndex==7 ) b_prime = 106.245; // AuAu 5.2GeV FXT Run20
    if(mParameterIndex==8 ) b_prime = 114.041; // AuAu 6.2GeV FXT Run20
    lumiCorr = (par0_lum<std::numeric_limits<double>::epsilon() ) ? 1.0 : b_prime/(par0_lum+zdcCoincidenceRate*par1_lum);
  }
  else {
    lumiCorr = (par0_lum < std::numeric_limits<double>::epsilon() ) ? 1.0 : 1.0/(1.0 + par1_lum/par0_lum*zdcCoincidenceRate/1000.);
  }

  // from Run14, P16id, for VpdMB5/VPDMB30/VPDMB-noVtx, use refMult at ZdcX=30, other is at ZdcX=0;  
  // -->changed by xlchen@lbl.gov, Run16 ~ 50kHz
  if( 
     ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
      ) {
    float zdcmean = 0;
    if(mYear[mParameterIndex] == 2014) zdcmean = 30.;
    if(mYear[mParameterIndex] == 2016) zdcmean = 50.;
    lumiCorr = (par0_lum==0.0) ? lumiCorr : lumiCorr*(par0_lum+par1_lum*zdcmean)/par0_lum; 
  }

  if (mVerbose) {
    std::cout << "\tLuminosity correction factor: " << lumiCorr << std::endl;
    std::cout << "\t[DONE]\n";
  }
  return lumiCorr;
}

//________________
Double_t StRefMultCorr::vzCorrection(Double_t z) const {

  if ( mVerbose ) {
    std::cout << "Estimation of acceptance correction factor..." << std::endl;
    std::cout << "\t Vertex z position: " << z << " mParameterIndex: " << mParameterIndex << std::endl;

  }
  Double_t vzCorr = 1.;
  if ( mParameterIndex < 38 ) {
    if (mRefX == 6) { // refMult6
      if ( mParameterIndex == 0 ) { // O+O 200 GeV 2021
        // New Vz correction. All vz bins bins are normalize to that at the center
        vzCorr = oo200_run21_vzCorr_refMult[ getVzWindowForVzDepCentDef() ];
      } // if ( mParameterIndex == 0 ) { // O+O 200 GeV 2021

      else if ( mParameterIndex == 1 ) { // d+Au 200 GeV 2021
        // New Vz correction. All vz bins bins are normalize to that at the center
        vzCorr = dau200_run21_vzCorr_refMult[ getVzWindowForVzDepCentDef() ];
      } // else if ( mParameterIndex == 1 ) { // d+Au 200 GeV 2021

    } // if (mRefX == 6)
    else if (mRefX == 7) { // totnMIP
      if ( mParameterIndex == 0 ) { // O+O 200 GeV 2021
        // New Vz correction. All vz bins bins are normalize to that at the center
        vzCorr = oo200_run21_vzCorr_totnMIP[ getVzWindowForVzDepCentDef() ];
      } // if ( mParameterIndex == 0 ) { // O+O 200 GeV 2021
    }
    else {
      // Old correction based on the 6th-order polynomial fit of the high-end point 
      // fit of refMult for the given Vz range
      
      // par0 to par5 define the parameters of a polynomial to parametrize z_vertex dependence of RefMult
      const Double_t par0 = mPar_z_vertex[0][mParameterIndex];
      const Double_t par1 = mPar_z_vertex[1][mParameterIndex];
      const Double_t par2 = mPar_z_vertex[2][mParameterIndex];
      const Double_t par3 = mPar_z_vertex[3][mParameterIndex];
      const Double_t par4 = mPar_z_vertex[4][mParameterIndex];
      const Double_t par5 = mPar_z_vertex[5][mParameterIndex];
      const Double_t par6 = mPar_z_vertex[6][mParameterIndex];
      // This parameter is usually 0, it takes care for an additional efficiency, 
      // usually difference between phase A and phase B parameter 0
      const Double_t par7 = mPar_z_vertex[7][mParameterIndex]; 
      
      const Double_t  RefMult_ref = par0; // Reference mean RefMult at z=0
      const Double_t  RefMult_z   = par0 + par1*z + par2*z*z + par3*z*z*z + par4*z*z*z*z + par5*z*z*z*z*z + par6*z*z*z*z*z*z; // Parametrization of mean RefMult vs. z_vertex position
      if(RefMult_z > 0.0) {
        vzCorr = (RefMult_ref + par7)/RefMult_z;
      }
    }
  }
  else if ( mParameterIndex == 38 ) {  // Au+Au 19.6 GeV Run 19
    // New Vz correction. All vz bins bins are normalize to the one at the center
    vzCorr = auau19_run19_vzCorr[ getVzWindowForVzDepCentDef() ];
  } // else if ( mParameterIndex == 38 )
  else if ( mParameterIndex == 39 ) {
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau14_run19_vzCorr[ getVzWindowForVzDepCentDef() ];
  }
  else if ( mParameterIndex == 40 ) {  // Au+Au 200 GeV Run 19
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau200_run19_vzCorr[ getVzWindowForVzDepCentDef() ];
  }
  else if ( mParameterIndex == 41 ) {  // Au+Au 7.7 GeV Run 21
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau7_run21_vzCorr[ getVzWindowForVzDepCentDef() ];
  }
  else if ( mParameterIndex == 42 ) {  // Au+Au 9.2 GeV Run 20 TriggerID = 780020
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau9_trig2_run20_vzCorr[ getVzWindowForVzDepCentDef() ];
  }
  else if ( mParameterIndex == 43 ) {  // Au+Au 17.3 GeV Run 21
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau17_run21_vzCorr[ getVzWindowForVzDepCentDef() ];
  }
  else if ( mParameterIndex == 44 ) {  // Au+Au 11.5 GeV Run 20
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau11_run20_vzCorr[ getVzWindowForVzDepCentDef() ];
  }

  if (mVerbose) {
    std::cout << "\t Acceptance correction factor: " << vzCorr << std::endl;
    std::cout << "\t[DONE]\n";
  }
  return vzCorr;
}

//________________
Double_t StRefMultCorr::sampleRefMult(Int_t refMult) const {

  if (mVerbose) {
    std::cout << "Sampling refMult value: " << refMult << std::endl;
  }

  Double_t refMult_d = -9999.;
  if( mParameterIndex>=30 && mParameterIndex<=44 ) {
    refMult_d = (Double_t)refMult - 0.5 + gRandom->Rndm();
  }
  else {
    refMult_d = (Double_t)refMult + gRandom->Rndm();
  }

  if (mVerbose) {
    std::cout << "\tSampled refMult value: " << refMult_d << std::endl;
    std::cout << "\t[DONE]\n";
  }
  return refMult_d;
}

//________________
Double_t StRefMultCorr::getRefMultCorr(const UShort_t refMult,
                                       const Double_t z,
                                       const Double_t zdcCoincidenceRate,
                                       const UInt_t flag) const {
  
  if (mVerbose) {
    std::cout << "Start refMultCorr calculations" << std::endl
              << "Initial values refMult / vz / zdcX / flag: "
              << refMult << " / " << z << " / " << zdcCoincidenceRate << " / "
              << flag << std::endl; 
  }
  // Apply correction if parameter index & z-vertex are ok
  if (!isIndexOk() || !isZvertexOk()) return refMult ;

  // Correction function for RefMult, takes into account z_vertex dependence

  Double_t lumiCorr = luminosityCorrection(zdcCoincidenceRate);
  Double_t vzCorr = vzCorrection(z);
  Double_t refMult_d = sampleRefMult( refMult );
  Double_t refMultCorr  = -9999. ;
  switch (flag) {
  case 0: refMultCorr = refMult_d * lumiCorr; break;
  case 1: refMultCorr =  refMult_d * vzCorr; break;
  case 2: refMultCorr = refMult_d * vzCorr * lumiCorr; break;
  default: {
    Error("StRefMultCorr::getRefMultCorr", "invalid flag, flag=%d, should be 0, 1 or 2", flag);
    refMultCorr = -9999.;
  }
  } // switch ( flag )

  if (mVerbose) {
    std::cout << "Final refMultCorr value: " << refMultCorr << std::endl;
  }

  return refMultCorr;
}

//_________________
void StRefMultCorr::readScaleForWeight(const Char_t* input) {
  std::ifstream fin(input) ;
  if(!fin) {
    Error("StRefMultCorr::readScaleForWeight", "can't open %s", input);
    return;
  }

  // Users must set the vz bin size by setVzForWeight() (see below)
  if(mnVzBinForWeight==0) {
    Error("StRefMultCorr::readScaleForWeight",
	  "Please call setVzForWeight() to set vz bin size");
    return;
  }

  // Do not allow multiple calls
  if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
    Error("StRefMultCorr::readScaleForWeight",
	  "scale factor has already set in the array");
    return;
  }

  std::cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..." << std::flush;
	
  while(fin) {
    Double_t scale[mnVzBinForWeight] ;
    for(Int_t i=0; i<mnVzBinForWeight; i++) {
      fin >> scale[i] ;
    }
		
    if(fin.eof()) break ;

    for(Int_t i=0; i<mnVzBinForWeight; i++) {
      mgRefMultTriggerCorrDiffVzScaleRatio.push_back(scale[i]);
    }
  }
  std::cout << " [OK]" << std::endl;
}

// NEW version to read Vz dependent weights from header
// Implemented inside StRefMultCorr::setParameterIndex(RunId)
//_________________
void StRefMultCorr::readScaleForWeight(const Int_t nRefmultbin, const Double_t *weight) {

  // Users must set the vz bin size by setVzForWeight() (see below)
  if( mnVzBinForWeight==0 ) {
    Error("StRefMultCorr::readScaleForWeight",
	  "Please call setVzForWeight() to set vz bin size");
    return;
  }

  // Do not allow multiple calls
  if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
    Error("StRefMultCorr::readScaleForWeight",
	  "scale factor has already set in the array");
    return;
  }

  std::cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..." << std::flush;
	
  for(Int_t i=0; i<nRefmultbin*mnVzBinForWeight; i++) {
    mgRefMultTriggerCorrDiffVzScaleRatio.push_back(weight[i]);
  }

  std::cout << " [OK]" << std::endl;
}


// In NEW version, setVzForWeight() is implemented inside StRefMultCorr::setParameterIndex(RunId)
// It does not need to be called by users.
//_________________
void StRefMultCorr::setVzForWeight(const Int_t nbin, const Double_t min, const Double_t max) {
  // Do not allow multiple calls
  if ( !mVzEdgeForWeight.empty() ) {
    Error("StRefMultCorr::setVzForWeight",
	  "z-vertex range for weight has already been defined");
    return;
  }

  mnVzBinForWeight = nbin ;
  // calculate increment size
  const Double_t step = (max-min)/(Double_t)nbin;
	
  for(Int_t i=0; i<mnVzBinForWeight+1; i++) {
    mVzEdgeForWeight.push_back( min + step*i );
  }
	
  // Debug
  for(Int_t i=0; i<mnVzBinForWeight; i++) {
    std::cout << i << " " << step << " " << mVzEdgeForWeight[i]
	            << ", " << mVzEdgeForWeight[i+1] << std::endl;
  }
}

//_________________
Double_t StRefMultCorr::getScaleForWeight() const {
  // Special scale factor for global refmult in Run14 (Run16)
  // to account for the relative difference of VPDMB5 w.r.t VPDMB30 (VPDMBnoVtx) 

  // return 1 if mgRefMultTriggerCorrDiffVzScaleRatio array is empty
  if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) return 1.0 ;

  //  const Int_t nVzBins =6;
  //  Double_t VzEdge[nVzBins+1]={-6., -4., -2., 0., 2., 4., 6.};

  Double_t VPD5weight=1.0;
  for(Int_t j=0;j<mnVzBinForWeight;j++) {
    if(mVz>mVzEdgeForWeight[j] && mVz<=mVzEdgeForWeight[j+1]) {
      /*
      //refMultbin=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->FindBin(mRefMult_corr+1e-6);
      //VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->GetBinContent(refMultbin);
      const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
      //VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[j][refMultbin];
      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
      const Double_t tmpContent=VPD5weight;
      if(tmpContent==0 || (mRefMult_corr>500 && tmpContent<=0.65)) VPD5weight=1.15;//Just because the value of the weight is around 1.15
      if(mRefMult_corr>500 && tmpContent>=1.35) VPD5weight=1.15;//Remove those Too large weight factor,gRefmult>500
      // this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
      */

      const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
      const Double_t tmpContent=VPD5weight;
      // 1) Ratios fluctuate too much at very high gRefmult due to low statistics
      // 2) Avoid some events with too high weight
      if(mRefMult_corr>550 && (tmpContent>3.0||tmpContent<0.3)) VPD5weight=1.0;
      // this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
    }
  }

  return 1.0/VPD5weight;
}

//_________________
// For Run18 27 GeV
Double_t StRefMultCorr::getShapeWeight_SubVz2Center() const {

  if (mVerbose) {
    std::cout << "Estimating shape correction factor" << std::endl;
  }

  Double_t weight = 1.;
  Int_t iVzBinIndex = getVzWindowForVzDepCentDef();

  if ( mParameterIndex>=30 && mParameterIndex<=35 ) { // Run18 27 GeV MB

    if(mRefMult_corr>=500.) return 1.0; // almost no Refmult>500 for this collision energy
		
    Int_t iRunPdIndex = mParameterIndex-30;
    Int_t iRefmultBin = (Int_t)(mRefMult_corr/2.); //find the refmult bin matching to the Parameter bin, if binWidth=2
    //Int_t iRefmultBin = (Int_t)(mRefMult_corr);  //find the refmult bin matching to the Parameter bin, if binWidth=1
		
    if(iRunPdIndex<0 || iRunPdIndex>5)  return 1.0;
    if(iVzBinIndex<0 || iVzBinIndex>13) return 1.0;
		
    //----------------------------------------------------------
    //load the ShapeReweight factors in given RunPd and VzBin
    //----------------------------------------------------------
    std::vector<std::string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(iRunPdIndex,iVzBinIndex),',');
    if( iRefmultBin>=(Int_t)sParam_ShapeWeight.size() ) {
      std::cout<<"ERROR: sParam_ShapeWeight is out of ranges!!!!!"<<std::endl;
      return 1.0;
    }
	
    //std::cout<<"sParam_ShapeWeight.size(): "<<sParam_ShapeWeight.size()<<std::endl;
    //for(UInt_t is=0; is<sParam_ShapeWeight.size(); is++) std::cout<<"sParam_ShapeWeight[is]: "<<sParam_ShapeWeight[is]<<std::endl;

    Double_t ShapeReweight = 1.0;

    Double_t tem_ShapeReweight = std::stod( sParam_ShapeWeight[iRefmultBin] );
    //prevent the crazy numbers for the large fluctuations
    if( tem_ShapeReweight<0.1 ) {
      ShapeReweight = 0.1;
    }
    else if(tem_ShapeReweight>10) {
      ShapeReweight = 10.;
    }
    else if(tem_ShapeReweight>=0.1 && tem_ShapeReweight<=10.) {
      ShapeReweight = tem_ShapeReweight;
    }
		
    weight = 1./ShapeReweight;

  } // if ( mParameterIndex>=30 && mParameterIndex<=35 ) { // Run18 27 GeV MB
  else if ( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobar collision 200 GeV 2018

    if (mVz >= -9 && mVz <= 9) {
      return 1.;
    }

    Int_t mShapeIndex = 0;
    if (mIsZr) {
      mShapeIndex = 1;
    }

    //retrive shape weight
    if (iVzBinIndex >= 22) {
      weight = ShapeWeightArray[mShapeIndex][iVzBinIndex - 9][TMath::Nint(mRefMult_corr)];
    }
    else {
      weight = ShapeWeightArray[mShapeIndex][iVzBinIndex][TMath::Nint(mRefMult_corr)];
    }
    //handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }                                 // else if ( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobar collision 200 GeV 2018
  else if (mParameterIndex == 38) { // Au+Au 19.6 GeV 2019

    if (iVzBinIndex < 0 || iVzBinIndex > auau19_run19_nVzBins) return 1.0;

    weight = auau19_run19_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if ( mParameterIndex == 38 ) {  // Au+Au 19.6 GeV 2019
  else if (mParameterIndex == 39) { // Au+Au 14.6 GeV 2019

    if (iVzBinIndex < 0 || iVzBinIndex > auau14_run19_nVzBins) return 1.0;

    weight = auau14_run19_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if ( mParameterIndex == 39 ) {  // Au+Au 14.6 GeV 2019
  else if (mParameterIndex == 40) { // Au+Au 200 GeV 2019

    if (iVzBinIndex < 0 || iVzBinIndex > auau200_run19_nVzBins) return 1.0;

    weight = auau200_run19_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }
  else if (mParameterIndex == 41) { // Au+Au 7.7 GeV 2020

    if (iVzBinIndex < 0 || iVzBinIndex > auau7_run21_nVzBins) return 1.0;

    weight = auau7_run21_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }
  else if (mParameterIndex == 42) { // Au+Au 9.2 GeV 2020 TrigerID = 780020

    if (iVzBinIndex < 0 || iVzBinIndex > auau9_run20_nVzBins) return 1.0;

    weight = auau9_trig2_run20_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }
  else if (mParameterIndex == 43) { // Au+Au 17.3 GeV 2021

    if (iVzBinIndex < 0 || iVzBinIndex > auau17_run21_nVzBins) return 1.0;

    weight = auau17_run21_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }
  else if (mParameterIndex == 44) { // Au+Au 11.5 GeV 2020

    if (iVzBinIndex < 0 || iVzBinIndex > auau11_run20_nVzBins) return 1.0;

    weight = auau11_run20_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }

  else if (mRefX == 6 && mParameterIndex == 0) { // O+O 200 GeV 2021

    if (iVzBinIndex < 0 || iVzBinIndex > oo200_run21_nVzBins) return 1.0;

    weight = oo200_run21_shapeWeightArray_refMult[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if (mRefX == 6 && mParameterIndex == 0) { // O+O 200 GeV 2021

  else if (mRefX == 7 && mParameterIndex == 0) { // O+O 200 GeV 2021

    if (iVzBinIndex < 0 || iVzBinIndex > oo200_run21_nVzBins) return 1.0;

    weight = oo200_run21_shapeWeightArray_totnMIP[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if (mRefX == 7 && mParameterIndex == 0) { // O+O 200 GeV 2021

  else if (mRefX == 6 && mParameterIndex == 1) { // d+Au 200 GeV 2021

    if (iVzBinIndex < 0 || iVzBinIndex > dau200_run21_nVzBins) return 1.0;

    weight = dau200_run21_shapeWeightArray_refMult[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if (mRefX == 6 && mParameterIndex == 1) { // d+Au 200 GeV 2021

  else {
    weight = 1.0;
  }

  if (mVerbose) {
    std::cout << "\tShape weight: " << weight << std::endl;
    std::cout << "\t[DONE]\n";
  }
  
  return weight;
}

//________________
Double_t StRefMultCorr::triggerWeight() const {

  if (mVerbose) {
    std::cout << "Estimating the trigger weight" << std::endl;
  }
  
  Double_t weight = 1.;

  const Double_t par0 =   mPar_weight[0][mParameterIndex];
  const Double_t par1 =   mPar_weight[1][mParameterIndex];
  const Double_t par2 =   mPar_weight[2][mParameterIndex];
  const Double_t par3 =   mPar_weight[3][mParameterIndex];
  const Double_t par4 =   mPar_weight[4][mParameterIndex];
  const Double_t A    =   mPar_weight[5][mParameterIndex]; // Set to 0 if no z-dependent trigger weight is set
  const Double_t par6 =   mPar_weight[6][mParameterIndex];
  const Double_t par7 =   mPar_weight[7][mParameterIndex];

  if (mVerbose) {
    std::cout << "Trigger efficiency parameters: "
              << " [0]: " << par0
              << " [1]: " << par1
              << " [2]: " << par2
              << " [3]: " << par3 
              << " [4]: " << par4
              << "  A: "  << A 
              << " [6]: " << par6
              << " [7]: " << par7 << std::endl;
  }

  // Additional z-vetex dependent correction
  //const Double_t A = ((1.27/1.21))/(30.0*30.0); // Don't ask...
  //const Double_t A = (0.05/0.21)/(30.0*30.0); // Don't ask...


  if ( isRefMultOk() // 0-80%
       && mRefMult_corr < mNormalize_stop[mParameterIndex] // reweighting only apply up to normalization point
       && mRefMult_corr != -(par3/par2)  ) { // avoid denominator = 0 

    // Parametrization of MC/data RefMult ratio
    if (mRefX == 5 && mParameterIndex == 0) { // Run 18 Au+Au 3.85 GeV (sqrt(s_NN)=3 GeV)
      // Trigger efficiency correction does not exist. Temporarily set weight to 1
      weight = 1.;
    } // else if (mRefX == 5 && mParameterIndex == 0)
    else if ((mRefX == 6 || mRefX == 7) && (mParameterIndex == 0 || mParameterIndex == 1)) { // O+O and d+Au 200 GeV 2021 
      weight = 1. + par0 * TMath::Exp(par1*mRefMult_corr + par2);
    } // else if ((mRefX == 6 || mRefX == 7) && (mParameterIndex == 0 || mParameterIndex == 1)) { // O+O and d+Au 200 GeV 2021 
    else {
      weight = ( par0 +
                par1 / (par2 * mRefMult_corr + par3) +
                par4 * (par2 * mRefMult_corr + par3) +
                par6 / ( (par2 * mRefMult_corr + par3) * (par2 * mRefMult_corr + par3) ) +
                par7 * ( (par2 * mRefMult_corr + par3) * (par2 * mRefMult_corr + par3) ) );
    }
    /*
    std::cout << "par0: " << par0 << " par1: " << par1 << " par2: " << par2
              << " par3: " << par3 << " par4: " << par4 << " A: " << A
              << " par6: " << par6 << " par7: " << par7 << "\n"
              << "refMultCorr: " << mRefMult_corr << " weight: " << weight << std::endl;
              */

    weight = weight + (weight - 1.0) * (A * mVz * mVz); // z-dependent weight correction
  }
  if (mVerbose) {
    std::cout << "\tTrigger weight: " << weight << std::endl;
    std::cout << "\t[DONE]\n";
  }
  return weight;
}

//_________________
Double_t StRefMultCorr::getWeight() const {

  Double_t Weight = 1.0;

  // Invalid index
  if( !isIndexOk() )   return Weight ;
  // Invalid z-vertex
  if( !isZvertexOk() ) return Weight ;

  // Trigger efficiency
  Weight = triggerWeight();

  //------------for Run14 and Run16----------------
  // Special scale factor for global refmult depending on Vz window
  // for others, scale factor = 1
  Weight *= getScaleForWeight();

  // Shape correction
  Weight *= getShapeWeight_SubVz2Center();

  return Weight;
}

//_________________
Int_t StRefMultCorr::getCentralityBin16() const {

  if (mVerbose) {
    std::cout << "Finding centrality bin (out of 16)\n";
  }
  Int_t CentBin16 = -1;

  // Invalid index
  if( !isIndexOk() ) return CentBin16 ;
  
  while( CentBin16 < mNCentrality && !isCentralityOk(CentBin16) ) {
    CentBin16++;
  }

  // Vz dependent centrality definition for Vz<-27 and Vz>25
  // Run17 54.4 GeV, trigid = 580001
  if( mParameterIndex==28&&(mVz<-27||mVz>25) ) {
    CentBin16 = getCentralityBin16VzDep();
    // if(CentBin16==-1) std::cout <<"Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr << std::endl;
    // if(CentBin16==9999) std::cout << "Invalide number"<< std::endl; 
    // std::cout <<"Vz dependent centrality definition for Vz<-27 and Vz>25 ... Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr <<" iCent="<< CentBin16 << std::endl;
  }

  if (mVerbose) {
    std::cout << "\tCentrality bin (out of 16): " << CentBin16 << "\t[DONE]" << std::endl;
  }

  // return -1 if CentBin16 = 16 (very large refmult, refmult>5000)
  return ( CentBin16==16 ) ? -1 : CentBin16;
}

//_________________
Int_t StRefMultCorr::getCentralityBin9() const {

  if (mVerbose) {
    std::cout << "Finding centrality bin (out of 9)\n";
  }

  Int_t CentBin9 = -1;

  // Invalid index
  if ( !isIndexOk() ) return CentBin9 ;

  const Int_t  CentBin16 = getCentralityBin16(); // Centrality bin 16
  const Bool_t isCentralityOk = CentBin16 >= 0 && CentBin16 < mNCentrality ;

  // No centrality is defined
  if ( !isCentralityOk ) return CentBin9;

  // First handle the exceptions
  if( mRefMult_corr > mCentrality_bins[15][mParameterIndex] && 
      mRefMult_corr <= mCentrality_bins[16][mParameterIndex] ) {
    CentBin9 = 8; // most central 5%
  }
  else if( mRefMult_corr > mCentrality_bins[14][mParameterIndex] && 
	   mRefMult_corr <= mCentrality_bins[15][mParameterIndex]) {
    CentBin9 = 7; // most central 5-10%
  }
  else {
    CentBin9 = (Int_t)(0.5*CentBin16);
  }

  // Vz dependent centrality definition for Vz<-27 and Vz>25
  // Run17 54.4 GeV, trigid = 580001
  // std::cout << mParameterIndex << std::endl;
  if ( mParameterIndex == 28 && ( mVz<-27 || mVz>25 ) ) {
    CentBin9 = getCentralityBin9VzDep();
  }

  if (mVerbose) {
    std::cout << "\tCentrality bin (out of 9): " << CentBin9 << "\t[DONE]" << std::endl;
  }

  return CentBin9;
}

//_________________
Int_t StRefMultCorr::getVzWindowForVzDepCentDef() const {
  Int_t iBinVz = -1;

  if( mParameterIndex==28 ) {	//  54.4 GeV, RefMult, 580001
    if( mVz>-30 && mVz<-29 )      iBinVz = 0;
    else if( mVz>-29 && mVz<-27 ) iBinVz = 1;
    else if( mVz>25 && mVz<27 )   iBinVz = 2;
    else if( mVz>27 && mVz<29 )   iBinVz = 3;
    else if( mVz>29 && mVz<30 )   iBinVz = 4;
    else iBinVz = -1;
  }
  else if( mParameterIndex>=30 && mParameterIndex<=35 ) { //Run18 27 GeV MB, 6 triggerIds
    if( mVz>=-70. && mVz<-60. ) iBinVz = 0;
    else if( mVz>=-60.&&mVz<-50.) iBinVz = 1;
    else if( mVz>=-50.&&mVz<-40.) iBinVz = 2;
    else if( mVz>=-40.&&mVz<-30.) iBinVz = 3;
    else if( mVz>=-30.&&mVz<-20.) iBinVz = 4;
    else if( mVz>=-20.&&mVz<-10.) iBinVz = 5;
    else if( mVz>=-10.&&mVz<0.0 ) iBinVz = 6;
    else if( mVz>=0.0 &&mVz<10. ) iBinVz = 7;
    else if( mVz>=10. &&mVz<20. ) iBinVz = 8;
    else if( mVz>=20. &&mVz<30. ) iBinVz = 9;
    else if( mVz>=30. &&mVz<40. ) iBinVz = 10;
    else if( mVz>=40. &&mVz<50. ) iBinVz = 11;
    else if( mVz>=50. &&mVz<60. ) iBinVz = 12;
    else if( mVz>=60. &&mVz<=70 ) iBinVz = 13;
    else iBinVz = -1;
  }
  else if( mParameterIndex>=36 && mParameterIndex<=37 ) { //Run18 200 GeV isobar 
    Double_t VtxZBinDouble = mVz/2. + 17.;
    iBinVz = 0;
    if (mVz == 25.) {
      iBinVz = 29;
    }
    else if (mVz == -35.) {
      iBinVz = 0;
    }
    else {
      iBinVz = TMath::Nint(VtxZBinDouble);
    }
  }
  else if ( (mParameterIndex == 38) || (mParameterIndex == 39) ) {  // Au+Au 19.6 GeV 2019 || Au+Au 14.6 GeV 2019

    for ( Int_t iVz=0; iVz<auau19_run19_nVzBins; iVz++ ) { // Utilize same Vz binning: (29 bins, -145., 145.)
      if ( auau19_run19_vzRangeLimits[iVz][0] <= mVz && mVz < auau19_run19_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau19_run19_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 38 )
  else if ( mParameterIndex == 40 ) {  // Au+Au 200 GeV 2019
    for ( Int_t iVz=0; iVz<auau200_run19_nVzBins; iVz++ ) { // Utilize Vz binning: (20 bins, -100., 100.)
      if ( auau200_run19_vzRangeLimits[iVz][0] <= mVz && mVz < auau200_run19_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau200_run19_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 40 )
  else if ( mParameterIndex == 41 ) {  // Au+Au 7.7 GeV 2020
    for ( Int_t iVz=0; iVz<auau7_run21_nVzBins; iVz++ ) { // Utilize Vz binning: (29 bins, -145., 145.)
      if ( auau7_run21_vzRangeLimits[iVz][0] <= mVz && mVz < auau7_run21_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau7_run21_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 41 )
  else if ( mParameterIndex == 42 ) {  // Au+Au 9.2 GeV Run 20 TriggerID = 780020
    for ( Int_t iVz=0; iVz<auau9_run20_nVzBins; iVz++ ) { // Utilize Vz binning: (29 bins, -145., 145.)
      if ( auau9_run20_vzRangeLimits[iVz][0] <= mVz && mVz < auau9_run20_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau9_run20_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 42 )
  else if ( mParameterIndex == 43 ) {  // Au+Au 17.3 GeV 2021
    for ( Int_t iVz=0; iVz<auau17_run21_nVzBins; iVz++ ) { // Utilize Vz binning: (29 bins, -145., 145.)
      if ( auau17_run21_vzRangeLimits[iVz][0] <= mVz && mVz < auau17_run21_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau17_run21_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 43 )
  else if ( mParameterIndex == 44 ) {  // Au+Au 11.5 GeV 2020
    for ( Int_t iVz=0; iVz<auau11_run20_nVzBins; iVz++ ) { // Utilize Vz binning: (29 bins, -145., 145.)
      if ( auau11_run20_vzRangeLimits[iVz][0] <= mVz && mVz < auau11_run20_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau17_run21_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 44 )

  else if ( (mRefX == 6 || mRefX == 7) && mParameterIndex == 0 ) {  // O+O 200 GeV 2021
    for ( Int_t iVz=0; iVz<oo200_run21_nVzBins; iVz++ ) {
      if ( oo200_run21_vzRangeLimits[iVz][0] <= mVz && mVz < oo200_run21_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<oo200_run21_nVzBins; iVz++ )
  } // else if ( (mRefX == 6 || mRefX == 7) && mParameterIndex == 0 ) {  // O+O 200 GeV 2021

  else if ( mRefX == 6 && mParameterIndex == 1 ) {  // d+Au 200 GeV 2021
    for ( Int_t iVz=0; iVz<dau200_run21_nVzBins; iVz++ ) {
      if ( dau200_run21_vzRangeLimits[iVz][0] <= mVz && mVz < dau200_run21_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<dau200_run21_nVzBins; iVz++ )
  } // else if ( mRefX == 6 && mParameterIndex == 1 ) {  // d+Au 200 GeV 2021

  else {
    iBinVz = -1;
  }
	
  return iBinVz;
}

//_________________
Int_t StRefMultCorr::getCentralityBin9VzDep() const {
  const Int_t vzid = getVzWindowForVzDepCentDef();
  Int_t iCent = 9999;
  for(Int_t i=0; i<9; i++) {
    if ( i==8 ) {
      if( mRefMult_corr>CentBin9_vzdep[vzid][i] && mRefMult_corr<50000 ) iCent = i; 
    }
    else if( mRefMult_corr>CentBin9_vzdep[vzid][i] && 
	     mRefMult_corr<CentBin9_vzdep[vzid][i+1] ) iCent = i; 
  }
  // 80-100% for icent=-1
  if( mRefMult_corr>0 && mRefMult_corr<CentBin9_vzdep[vzid][0] ) iCent = -1; 
  return ( iCent==9999 ) ? -1 : iCent;
}

//_________________
Int_t StRefMultCorr::getCentralityBin16VzDep() const {
  const Int_t vzid = getVzWindowForVzDepCentDef();
  Int_t iCent = 9999;
  for(Int_t i=0; i<16; i++) {
    if (i == 15) {
      if (mRefMult_corr > CentBin16_vzdep[vzid][i] && mRefMult_corr < 50000) {
        iCent = i;
      }
    }
    else if (mRefMult_corr > CentBin16_vzdep[vzid][i] &&
             mRefMult_corr < CentBin16_vzdep[vzid][i + 1]) {
      iCent = i;
    }
  }
  // 80-100% for icent=-1
  if( mRefMult_corr>0 && mRefMult_corr<CentBin16_vzdep[vzid][0] ) {
    iCent = -1; 
  }
  return (iCent==9999) ? -1 : iCent;
}

//_________________
const Int_t StRefMultCorr::getRefX() const {
  if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return 0; 
  else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return 1; 
  else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return 2; 
  else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return 3; 
  else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return 4;
  else if ( mName.CompareTo("fxtmult",  TString::kIgnoreCase) == 0 ) return 5;
  else if ( mName.CompareTo("refmult6", TString::kIgnoreCase) == 0 ) return 6;
  else if ( mName.CompareTo("totnmip",  TString::kIgnoreCase) == 0 ) return 7;
  else return 9999;
}

//_________________
const Int_t StRefMultCorr::getNumberOfDatasets() const {
  if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return nID_gref; 
  else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return nID_ref1; 
  else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return nID_ref2; 
  else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return nID_ref3; 
  else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return nID_ref4;
  else if ( mName.CompareTo("fxtmult",  TString::kIgnoreCase) == 0 ) return nID_ref5; 
  else if ( mName.CompareTo("refmult6", TString::kIgnoreCase) == 0 ) return nID_ref6;
  else if ( mName.CompareTo("totnmip",  TString::kIgnoreCase) == 0 ) return nID_ref7;
  else return 9999;
}

//_________________
void StRefMultCorr::readHeaderFile() {

  //std::vector<std::string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(1,1),',');
  //for(int ib=0;ib<sParam_ShapeWeight.size(); ib++) std::cout<<"sParam_ShapeWeight[i]: "<<sParam_ShapeWeight[ib]<<std::endl;
  
  // Read reference multiplicity to work with (grefmult, refMult, refMult2, refMult3, fxtMult)
  const Int_t refX = getRefX();
  mRefX = refX;
  // Retrieve number of datasets for the given mRefX
  const Int_t nID =  getNumberOfDatasets();
  
  for(int iID=0; iID<nID; iID++) {

    //
    // Year, energy, run numbers, Vz cut
    //
    Int_t year; Double_t energy;
    std::vector<std::string> sParam = StringSplit(getParamX(refX,iID,0),':'); 
    year = stoi(sParam[0]); 
    energy = stoi(sParam[1]); 
    std::vector<std::string> sRuns = StringSplit(sParam[2],',');
		
    Int_t startRunId=0, stopRunId=0;
    startRunId = stoi(sRuns[0]); 
    stopRunId = stoi(sRuns[1]); 
		
    Double_t startZvertex=-9999., stopZvertex=-9999. ;
    std::vector<std::string> sVz = StringSplit(sParam[3],',');
    startZvertex = stod(sVz[0]);
    stopZvertex = stod(sVz[1]);

    mYear.push_back(year);
    mBeginRun.insert(std::make_pair(std::make_pair(energy, year), startRunId));
    mEndRun.insert(  std::make_pair(std::make_pair(energy, year), stopRunId));
    mStart_runId.push_back( startRunId ) ;
    mStop_runId.push_back(  stopRunId ) ;
    mStart_zvertex.push_back( startZvertex ) ;
    mStop_zvertex.push_back(  stopZvertex ) ;

    //
    // Centrality definition
    //
    std::vector<std::string> sParamCent = StringSplit(getParamX(refX,iID,1),','); 
    for(UInt_t i=0; i<sParamCent.size(); i++) {
      mCentrality_bins[i].push_back( stoi(sParamCent[i]) );
    }
    mCentrality_bins[mNCentrality].push_back( 5000 );

    //
    // Normalization range
    //
    Double_t normalize_stop=-1.0 ;
    normalize_stop = stod(getParamX(refX,iID,2)) ;
    mNormalize_stop.push_back( normalize_stop );

    //
    // Acceptance (vz) correction
    //
    std::vector<std::string> sParamVz = StringSplit(getParamX(refX,iID,3),','); 
    for(UInt_t i=0; i<mNPar_z_vertex; i++) {
      Double_t val = -9999.; 
      if(i<sParamVz.size()) val = stod(sParamVz[i]);
      else                  val = 0.0;
      mPar_z_vertex[i].push_back( val );
    }

    //
    // Trigger inefficiency correction
    //
    std::vector<std::string> sParamTrig = StringSplit(getParamX(refX,iID,4),','); 
    for(UInt_t i=0; i<mNPar_weight; i++) {
      Double_t val = -9999.;
      if(i<sParamTrig.size()) val = stod(sParamTrig[i]);
      else                    val = 0.0;
      mPar_weight[i].push_back( val );
    }

    //
    // Luminosity correction
    //
    std::vector<std::string> sParamLumi = StringSplit(getParamX(refX,iID,5),','); 
    for(UInt_t i=0; i<mNPar_luminosity; i++) {
      Double_t val = -9999.;
      if(i<sParamLumi.size()) val = stod(sParamLumi[i]);
      else                    val = 0.0;
      mPar_luminosity[i].push_back( val );
    }

    //	std::cout << refX <<"  "<< iID <<"/"<< nID << std::endl;
  }

  std::cout << "StRefMultCorr::readHeaderFile  [" << mName 
       << "] Correction parameters and centrality definitions have been successfully read for refX=" << refX
       << std::endl;
}

//_________________
void StRefMultCorr::readBadRunsFromHeaderFile() {

  //
  // TODO: Modify to read only bad runs associated with the requested year
  //

  for(Int_t i=0; i<nBadRun_refmult_2010; i++) {
    mBadRun.push_back(badrun_refmult_2010[i]);
  }
  std::cout << "read in nBadRun_refmult_2010: " << nBadRun_refmult_2010 << std::endl;

  for(Int_t i=0; i<nBadRun_refmult_2011; i++) {
    mBadRun.push_back(badrun_refmult_2011[i]);
  }
  std::cout << "read in nBadRun_refmult_2011: " << nBadRun_refmult_2011 << std::endl;

  for(Int_t i=0; i<nBadRun_grefmult_2014; i++) {
    mBadRun.push_back(badrun_grefmult_2014[i]);
  }
  std::cout << "read in nBadRun_grefmult_2014: " << nBadRun_grefmult_2014 << std::endl;

  for(Int_t i=0; i<nBadRun_grefmult_2016; i++) {
    mBadRun.push_back(badrun_grefmult_2016[i]);
  }
  std::cout << "read in nBadRun_grefmult_2016: " << nBadRun_grefmult_2016 << std::endl;

  for(Int_t i=0; i<nBadRun_refmult_2017; i++) {
    mBadRun.push_back(badrun_refmult_2017[i]);
  }
  std::cout << "read in nBadRun_refmult_2017: " << nBadRun_refmult_2017 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2018; i++) {
    mBadRun.push_back(badrun_refmult_2018[i]);
  }
  std::cout << "read in nBadRun_refmult_2018: " << nBadRun_refmult_2018 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2019; i++) {
    mBadRun.push_back(badrun_refmult_2019[i]);
  }
  std::cout << "read in nBadRun_refmult_2019: " << nBadRun_refmult_2019 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2020; i++) {
    mBadRun.push_back(badrun_refmult_2020[i]);
  }
  std::cout << "read in nBadRun_refmult_2020: " << nBadRun_refmult_2020 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2021; i++) {
    mBadRun.push_back(badrun_refmult_2021[i]);
  }
  std::cout << "read in nBadRun_refmult_2021: " << nBadRun_refmult_2021 << std::endl;

  //// notification only one
  if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) {
    std::cout << "StRefMultCorr::readBadRunsFromHeaderFile  Bad runs for year 2010, 2011, 2017, 2018 and 2019 have been read." << std::endl;
  }
}

//_________________
void StRefMultCorr::print(const Option_t* option) const {
  std::cout << "StRefMultCorr::print  Print input parameters for " 
            << mName << " ========================================" << std::endl << std::endl;
  // Option switched off, can be used to specify parameters
  //  const TString opt(option);
  
  //  Int_t input_counter = 0;
  for(UInt_t id=0; id<mStart_runId.size(); id++) {
    //std::cout << "Data line = " << input_counter << ", Start_runId = " << Start_runId[input_counter] << ", Stop_runId = " << Stop_runId[input_counter] << std::endl;
    //    const UInt_t id = mStart_runId.size()-1;
    
    // Removed line break
    std::cout << "  Index=" << id;
    std::cout << Form(" Run=[%8d, %8d]", mStart_runId[id], mStop_runId[id]);
    std::cout << Form(" z-vertex=[%1.1f, %1.1f]", mStart_zvertex[id], mStop_zvertex[id]);
    std::cout << ", Normalize_stop=" << mNormalize_stop[id];
    std::cout << std::endl;
    
    //    if(opt.IsWhitespace()){
    //      continue ;
    //    }
    
    std::cout << "Centrality:  ";
		
    for(Int_t i=0;i<mNCentrality;i++) {
      std::cout << Form("  >%2d%%", 80-5*i);
    }
    std::cout << std::endl;
    std::cout << "RefMult:     ";
		
    for(Int_t i=0;i<mNCentrality;i++) {
      //      std::cout << Form("StRefMultCorr::read  Centrality %3d-%3d %%, refmult > %4d", 75-5*i, 80-5*i, mCentrality_bins[i][id]) << std::endl;
      const TString tmp(">");
      const TString centrality = tmp + Form("%d", mCentrality_bins[i][id]);
      std::cout << Form("%6s", centrality.Data());
    }
    std::cout << std::endl;

    for(Int_t i=0;i<mNPar_z_vertex;i++) {
      std::cout << "  mPar_z_vertex[" << i << "] = " << mPar_z_vertex[i][id];
    }
    std::cout << std::endl;
		
    for(Int_t i=0;i<mNPar_weight;i++) {
      std::cout << "  mPar_weight[" << i << "] = " << mPar_weight[i][id];
    }
    std::cout << std::endl;
    
    for(Int_t i=0;i<mNPar_luminosity;i++) {
      std::cout << "  mPar_luminosity[" << i << "] = " << mPar_luminosity[i][id];
    }
    std::cout << std::endl << std::endl;
  }
  std::cout << "=====================================================================================" << std::endl;
}

//_________________
std::vector<std::string> StRefMultCorr::StringSplit( const std::string str, const char sep ) const {
  std::vector<std::string> vstr;
  std::stringstream ss(str);
  std::string buffer;
  while( getline(ss,buffer,sep) ) {
    vstr.push_back(buffer);
  }
  return vstr;
}

//________________
Double_t StRefMultCorr::calcPileUpRefMult(Double_t ntofmatch, Double_t x0, Double_t x1, 
                                          Double_t x2, Double_t x3, Double_t x4) const {
  return ( x0 + x1*(ntofmatch) + x2*pow(ntofmatch,2) + x3*pow(ntofmatch,3) + x4*pow(ntofmatch,4) );
}
 
//________________
Bool_t StRefMultCorr::isPileUpEvent(Double_t refmult, Double_t ntofmatch, Double_t vz, Double_t totnMIP) const {
  if ((mRefX==6) || (mRefX==7)) {
    // refMult6 and totnMIP require both refMult vs. nBTOFMatch and totnMIP vs. nBTOFMatch for pileup rejection
    if (totnMIP < 0.) {
      Error("StRefMultCorr::isPileUpEvent", "totnMIP<0");
      return kTRUE;
    } // if (totnMIP < 0.)
    return !( passnTofMatchRefmultCut(refmult, ntofmatch, vz) && passnTofMatchTotnMIPCut(totnMIP, ntofmatch, vz) );
  } // if ((mRefX==6) || (mRefX==7))
  else { // other refMult
    return !passnTofMatchRefmultCut(refmult, ntofmatch, vz);
  }
}