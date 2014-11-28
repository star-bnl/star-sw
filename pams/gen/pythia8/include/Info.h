// Info.h is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This file contains a class that keep track of generic event info.
// Info: contains information on the generation process and errors.

#ifndef Pythia8_Info_H
#define Pythia8_Info_H

#include "PythiaStdlib.h"

namespace Pythia8 {
 
//**************************************************************************

// The Info class contains a mixed bag of information on the event
// generation activity, especially on the current subprocess properties,
// and on the number of errors encountered. This is used by the 
// generation machinery, but can also be read by the user.

class Info {

public:

  // Constructor. 
  Info() {} 

  // Listing of most available information on current event.
  void   list(ostream& os = cout);
  
  // Beam particles (in rest frame). CM energy of event.
  int    idA()            const {return idASave;}
  int    idB()            const {return idBSave;}
  double pzA()            const {return pzASave;}
  double pzB()            const {return pzBSave;}
  double eA()             const {return eASave;}
  double eB()             const {return eBSave;}
  double mA()             const {return mASave;}
  double mB()             const {return mBSave;}
  double eCM()            const {return eCMSave;}
  double s()              const {return sSave;}

  // Process name and code, and the number of final-state particles.
  string name()           const {return nameSave;}
  int    code()           const {return codeSave;}    
  int    nFinal()         const {return nFinalSave;}

  // Are beam particles resolved, with pdf's? Are they diffractive? 
  bool   isResolved()     const {return isRes;}
  bool   isDiffractiveA() const {return isDiffA;} 
  bool   isDiffractiveB() const {return isDiffB;} 
  bool   isMinBias()      const {return isMB;}

  // Information for Les Houches Accord and reading files.
  bool   isLHA()          const {return isLH;}
  bool   atEndOfFile()    const {return atEOF;}

  // For minbias and Les Houches Accord identify hardest subprocess.
  bool   hasSub()         const {return hasSubSave;}
  string nameSub()        const {return nameSubSave;}
  int    codeSub()        const {return codeSubSave;}    
  int    nFinalSub()      const {return nFinalSubSave;}

  // Incoming parton flavours and x values.
  int    id1()            const {return id1Save;}
  int    id2()            const {return id2Save;}
  double x1()             const {return x1Save;}
  double x2()             const {return x2Save;}
  double y()              const {return 0.5 * log( x1Save / x2Save );}
  double tau()            const {return x1Save * x2Save;}

  // Incoming parton densities, hard process couplings, Q2 scales.
  double pdf1()           const {return pdf1Save;}
  double pdf2()           const {return pdf2Save;}
  double QFac()           const {return sqrtpos(Q2FacSave);}
  double Q2Fac()          const {return Q2FacSave;}
  bool   isValence1()     const {return isVal1;}
  bool   isValence2()     const {return isVal2;}
  double alphaS()         const {return alphaSSave;}
  double alphaEM()        const {return alphaEMSave;}
  double QRen()           const {return sqrtpos(Q2RenSave);}
  double Q2Ren()          const {return Q2RenSave;}

  // Mandelstam variables (notation as if subcollision).
  double mHat()           const {return sqrt(sH);}   
  double sHat()           const {return sH;}   
  double tHat()           const {return tH;}   
  double uHat()           const {return uH;}   
  double pTHat()          const {return pTH;} 
  double pT2Hat()         const {return pTH*pTH;} 
  double m3Hat()          const {return m3H;}   
  double m4Hat()          const {return m4H;} 
  double thetaHat()       const {return thetaH;}   
  double phiHat()         const {return phiH;}   

  // Weight of current event; normally 1, but used for Les Houches events.
  double weight()         const {return weightSave;}   

  // Cross section estimate.
  long   nTried()         const {return nTry;}
  long   nSelected()      const {return nSel;}
  long   nAccepted()      const {return nAcc;}
  double sigmaGen()       const {return sigGen;}
  double sigmaErr()       const {return sigErr;}

  // Impact parameter picture.
  double bMI()            const {return (bIsSet) ? bMISave : 1.;}
  double enhanceMI()      const {return (bIsSet) ? enhanceMISave : 1.;}

  // Maximum pT scales for MI, ISR and FSR (in hard process).
  double pTmaxMI()        const {return pTmaxMISave;}
  double pTmaxISR()       const {return pTmaxISRSave;}
  double pTmaxFSR()       const {return pTmaxFSRSave;}

  // Number of multiple interactions, with code and pT for them.
  int    nMI()            const {return nMISave;}
  int    codeMI(int i)    const {return codeMISave[i];} 
  double pTMI(int i)      const {return pTMISave[i];} 

  // Number of times other steps have been carried out.
  int    nISR()           const {return nISRSave;}
  int    nFSRinProc()     const {return nFSRinProcSave;}
  int    nFSRinRes()      const {return nFSRinResSave;}

  // Reset to empty map of error messages.
  void errorReset() {messages.clear();}
  
  // Print a message the first few times. Insert in database.
  void errorMsg(string messageIn, string extraIn = " ",
    ostream& os = cout);

  // Provide total number of errors/aborts/warnings experienced to date.
  int  errorTotalNumber();

  // Print statistics on errors/aborts/warnings.
  void errorStatistics(ostream& os = cout);

private:

  // Store common beam quantities. 
  int    idASave, idBSave;
  double pzASave, eASave,mASave, pzBSave, eBSave, mBSave, eCMSave, sSave;

  // Store common integrated cross section quantities.
  long   nTry, nSel, nAcc;
  double sigGen, sigErr;

  // Store current-event quantities.
  bool   isRes, isDiffA, isDiffB, isMB, isLH, hasSubSave, bIsSet, evolIsSet,
         atEOF, isVal1, isVal2;  
  int    codeSave, codeSubSave, nFinalSave, nFinalSubSave, nTotal, 
         id1Save, id2Save, nMISave, nISRSave, nFSRinProcSave, nFSRinResSave;
  double x1Save, x2Save, pdf1Save, pdf2Save, Q2FacSave, alphaEMSave, 
         alphaSSave, Q2RenSave, sH, tH, uH, pTH, m3H, m4H, thetaH, phiH, 
         weightSave, bMISave, enhanceMISave, pTmaxMISave, pTmaxISRSave, 
         pTmaxFSRSave;
  string nameSave, nameSubSave;
  vector<int>    codeMISave;
  vector<double> pTMISave;

  // Friend classes allowed to set info.
  friend class Pythia;
  friend class ProcessLevel;
  friend class ProcessContainer;
  friend class PartonLevel;
  friend class MultipleInteractions;

  // Set info on the two incoming beams: only from Pythia class.
  void setBeamA( int idAin, double pzAin, double eAin, double mAin) {
    idASave = idAin; pzASave = pzAin; eASave = eAin; mASave = mAin;}
  void setBeamB( int idBin, double pzBin, double eBin, double mBin) {
    idBSave = idBin; pzBSave = pzBin; eBSave = eBin; mBSave = mBin;}
  void setECM( double eCMin) {eCMSave = eCMin; sSave = eCMSave * eCMSave;}

  // Reset info for current event: only from Pythia class.
  void clear() { isRes = isDiffA = isDiffB = isMB = isLH = atEOF = bIsSet 
    = isVal1 =isVal2 = false; codeSave = nFinalSave = nTotal = id1Save 
    = id2Save = nMISave = nISRSave = nFSRinProcSave = nFSRinResSave = 0; 
    x1Save = x2Save = pdf1Save = pdf2Save = Q2FacSave = alphaEMSave 
    = alphaSSave = Q2RenSave = sH = tH = uH = pTH = m3H = m4H = thetaH 
    = phiH = 0.; nameSave = " "; weightSave = bMISave = enhanceMISave = 1.; 
    codeMISave.resize(0); pTMISave.resize(0);}

  // Set info on the (sub)process: from ProcessLevel, ProcessContainer or 
  // MultipleInteractions classes.
  void setType( string nameIn, int codeIn, int nFinalIn,  
    bool isMinBiasIn = false, bool isResolvedIn = true, 
    bool isDiffractiveAin = false, bool isDiffractiveBin = false,
    bool isLHAin = false) {nameSave = nameIn; codeSave = codeIn; 
    nFinalSave = nFinalIn; isMB = isMinBiasIn; isRes = isResolvedIn; 
    isDiffA = isDiffractiveAin; isDiffB = isDiffractiveBin; isLH = isLHAin;
    nTotal = 2 + nFinalSave; bIsSet = false; hasSubSave = false; 
    nameSubSave = " "; codeSubSave = 0; nFinalSubSave = 0; evolIsSet = false;}
  void setSubType( string nameSubIn, int codeSubIn, int nFinalSubIn) {  
    hasSubSave = true; nameSubSave = nameSubIn; codeSubSave = codeSubIn; 
    nFinalSubSave = nFinalSubIn;}
  void setPDFalpha( int id1In, int id2In,  double pdf1In, double pdf2In, 
    double Q2FacIn, double alphaEMIn, double alphaSIn, double Q2RenIn) 
    {id1Save = id1In; id2Save = id2In; pdf1Save = pdf1In; pdf2Save = pdf2In; 
    Q2FacSave = Q2FacIn; alphaEMSave = alphaEMIn; alphaSSave = alphaSIn; 
    Q2RenSave = Q2RenIn;}
  void setKin( double x1In, double x2In, double sHatIn, double tHatIn, 
    double uHatIn, double pTHatIn, double m3HatIn, double m4HatIn, 
    double thetaHatIn, double phiHatIn) {x1Save = x1In; x2Save = x2In; 
    sH = sHatIn; tH = tHatIn; uH = uHatIn; pTH = pTHatIn; m3H = m3HatIn; 
    m4H = m4HatIn; thetaH = thetaHatIn; phiH = phiHatIn;}
  void setTypeMI( int codeMIIn, double pTMIIn) {
    codeMISave.push_back(codeMIIn); pTMISave.push_back(pTMIIn);}

  // Set info on cross section: from ProcessLevel.
  void setSigma( long nTryIn, long nSelIn, long nAccIn, double sigGenIn, 
    double sigErrIn) { nTry = nTryIn; nSel = nSelIn; nAcc = nAccIn; 
    sigGen = sigGenIn; sigErr = sigErrIn;} 

  // Set info on valence character of hard collision partons: from PartonLevel.
  void setValence( bool isVal1In, bool isVal2In) {isVal1 = isVal1In; 
    isVal2 = isVal2In;}

  // Set info on impact parameter: from PartonLevel.
  void setImpact( double bMIIn, double enhanceMIIn) {bMISave = bMIIn;
    enhanceMISave = enhanceMIIn, bIsSet = true;} 

  // Set info on pTmax scales and number of evolution steps: from PartonLevel.
  void setEvolution( double pTmaxMIIn, double pTmaxISRIn, double pTmaxFSRIn, 
    int nMIIn, int nISRIn, int nFSRinProcIn, int nFSRinResIn) { 
    pTmaxMISave = pTmaxMIIn; pTmaxISRSave = pTmaxISRIn; 
    pTmaxFSRSave = pTmaxFSRIn; nMISave = nMIIn; nISRSave = nISRIn; 
    nFSRinProcSave = nFSRinProcIn; nFSRinResSave = nFSRinResIn; 
    evolIsSet = true;}

  // Set info whether reading of Les Houches Accord file at end.
  void setEndOfFile( bool atEOFin) {atEOF = atEOFin;}

  // Set event weight; currently only for Les Houches description.
  void setWeight( double weightIn) {weightSave = weightIn;}

  // Number of times the same error message is repeated.
  static const int TIMESTOPRINT;

  // Map for all error messages.
  map<string, int> messages;

};
 
//**************************************************************************

} // end namespace Pythia8

#endif // Pythia8_Info_H
