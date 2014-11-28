#ifndef __SpinIUHistos_h__
#define __SpinIUHistos_h__
/**
 * \class  SpinIUHistos
 * \author Jason C. Webb, Weihong He 
 * \brief  Spin sorted pi0 histograms
 *
 * Class to contain spin-sorted pi0 histograms for analysis.  We inhereit
 * from TDirectory, and store all histograms w/in.  The directory should
 * be inserted into the owning maker's .histos directory.  Each instance
 * of this class is meant to store histograms for one spin state.
 *
 */

#include "TNamed.h"
#include "TDirectory.h"
class TH1F;
class TH2F;

#include "StEEmcIUPair.h"

class SpinIUHistos : public TDirectory 
{

 public:

  SpinIUHistos( const Char_t *name, const Char_t *title="spin sorted pi0" );
  ~SpinIUHistos(){ /* nada */ };

  void Fill( StEEmcIUPair &pair );

  /// Resets all spectra in the event that a spin sorting problem 
  /// is detected.
  void Clear(Option_t *opts="");

 private:
 protected:
  
  // cuts

  Float_t mMin;          /**<-- min mass for gated histograms */
  Float_t mMax;          /**<-- max mass for gated histograms */

  // integrated mass 

  TH1F *hMass;           /**<-- two photon inv mass */

  // mass vs other kinematics

  TH2F *hPT;             /**<-- PT vs mass */
  TH2F *hZgg;            /**<-- Zgg vs mass */
  TH2F *hZvert;          /**<-- z vertex vs mass */
  TH2F *hEta;            /**<-- eta vs mass */
  TH2F *hEEmcEta;  
  TH2F *hRGeo;
  TH2F *hPhi;
  // quantities gated on pi0 mass

  TH2F *hPhiggVsEnergy;  /**<-- Phigg vs energy */
  TH2F *hYX[3];          /**<-- Y vs X for pi0, each gamma */
  TH2F *hE1E2;           /**<-- E1 vs E2 */  
  TH2F *hEpi;  
  TH2F *hEsmd;           /**<-- E smd / E pi0 vs Epi0 */ 
  TH2F *hEpre1;          /**<-- E pre1 / E pi0 vs Epi0 */
  TH2F *hEpre2;          /**<-- E pre2 / E pi0 vs Epi0 */
  TH2F *hEpost;          /**<-- E post / E pi0 vs Epi0 */ 
  TH2F *hEpre12;         /**<-- E pre2 vs E pre1 */

  ClassDef(SpinIUHistos,1);

};

#endif
