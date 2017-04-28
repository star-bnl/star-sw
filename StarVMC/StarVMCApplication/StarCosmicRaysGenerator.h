// $Id: StarCosmicRaysGenerator.h,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
// $Log: StarCosmicRaysGenerator.h,v $
// Revision 1.1.1.1  2008/12/10 20:45:52  fisyak
// Merge with macos version
//
// Revision 1.1  2005/06/09 20:13:47  fisyak
// It looks like that all hits in place (calorimeters have to be check for volumeid)
//
// Revision 1.2  2005/05/03 15:42:14  fisyak
// Adjust for bfc
//
// Revision 1.1  2005/04/25 20:44:28  fisyak
// StarVMCApplication with example in macros/starVMC.C
//

#ifndef StarCosmicRaysGenerator_h
#define StarCosmicRaysGenerator_h
#include "StarMCPrimaryGenerator.h"
#include "TF1.h"
class StarCosmicRaysGenerator : public StarMCPrimaryGenerator  {
 public:
  StarCosmicRaysGenerator(StarStack* stack) : StarMCPrimaryGenerator() {fStarStack = stack; }
    StarCosmicRaysGenerator() {}
    virtual ~StarCosmicRaysGenerator() {}
  
  static StarCosmicRaysGenerator* Instance() {return (StarCosmicRaysGenerator*) StarMCPrimaryGenerator::Instance();}
  virtual void GeneratePrimaries();
  virtual void GeneratePrimaries(const TVector3& /* origin */) {}
 private:
  void GeneratePrimary();
  ClassDef(StarCosmicRaysGenerator,1)  //StarCosmicRaysGenerator
};
#endif //StarCosmicRaysGenerator_h

