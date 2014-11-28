#ifndef __SpinHistos_h__
#define __SpinHistos_h__
/**
 * \class  SpinHistos
 * \author Jason C. Webb 
 * \brief  Spin sorted pi0 histograms
 *
 * Class to contain spin-sorted pi0 histograms for analysis.  We inhereit
 * from TDirectory, and store all histograms w/in.  The directory should
 * be inserted into the owning maker's .histos directory.  Each instance
 * of this class is meant to store histograms for one spin state.
 *
 */

#include "TNamed.h"

class TH1F;
class TH2F;
#include "TDirectory.h"
#include "StEEmcPair.h"

class SpinHistos : public TDirectory 
{

 public:

  SpinHistos( const Char_t *name, const Char_t *title="spin sorted pi0" );
  ~SpinHistos(){ /* nada */ };

  void Fill(const StEEmcPair &pair );

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
  
  // quantities gated on pi0 mass

  TH2F *hPhiggVsEnergy;  /**<-- Phigg vs energy */
  TH2F *hYX[3];          /**<-- Y vs X for pi0, each gamma */
  TH2F *hE1E2;           /**<-- E1 vs E2 */  

  TH2F *hEsmd;           /**<-- E smd / E pi0 vs Epi0 */ 
  TH2F *hEpre1;          /**<-- E pre1 / E pi0 vs Epi0 */
  TH2F *hEpre2;          /**<-- E pre2 / E pi0 vs Epi0 */
  TH2F *hEpost;          /**<-- E post / E pi0 vs Epi0 */ 
  TH2F *hEpre12;         /**<-- E pre2 vs E pre1 */

  ClassDef(SpinHistos,1);

};

#endif
