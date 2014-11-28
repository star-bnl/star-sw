//---------------------------------------------------------------------------------
// The KFParticle class
// .
// @author  Maksym Zyzak
// @version 1.0
// @since   13.05.07
// 
// Class to reconstruct and store the decayed particle parameters.
// The method is described in CBM-SOFT note 2007-003, 
// ``Reconstruction of decayed particles based on the Kalman filter'', 
// http://www.gsi.de/documents/DOC-2007-May-14-1.pdf
//
// This class is ALICE interface to general mathematics in KFParticleBase
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//_________________________________________________________________________________

#ifndef KFParticleTest_H
#define KFParticleTest_H

class KFParticle;
class KFParticleSIMD;

class KFParticleTest
{
 public:
  
  KFParticleTest();
  ~KFParticleTest();
  
  void PrintTutorial();
  void RunTest();
  
 private:
   
  void RunTestSingle();
  void RunTestSIMD();
  void CompareSingleAndSIMDResults();
  
  KFParticle* fMotherSingle;
  KFParticleSIMD* fMotherSIMD;
  
  #ifdef KFParticleInRoot
  ClassDef( KFParticleTest, 1 )
  #endif
};

#endif //KFParticleTest_H

