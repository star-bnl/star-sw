// $Id: StarDetectorConstruction.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $
//

#ifndef STARDETECTORCONSTRUCTION_H
#define STARDETECTORCONSTRUCTION_H

#include <TObject.h>
#include <TString.h>
#include <TList.h>

class StarDetectorConstruction : public TObject
{
  public:  
    StarDetectorConstruction();
    virtual ~StarDetectorConstruction();

  public:
     void ConstructMaterials();
     void ConstructGeometry();
     
     // set methods
     void SetTargetMaterial (const TString& name);
     void SetChamberMaterial(const TString& name);

     Double_t GetTrackerFullLength() {return fTrackerLength;};
     Double_t GetTargetFullLength()  {return fTargetLength;};
     Double_t GetWorldFullLength()   {return fWorldLength;}; 
     
  private:
     Double_t  fWorldLength;   // Full length of the world volume
     Double_t  fTargetLength;  // Full length of Target
     Double_t  fTrackerLength; // Full length of Tracker
     Int_t     fNofChambers;   // Nb of chambers in the tracker region
     Double_t  fChamberWidth;  // Width of the chambers
     Double_t  fChamberSpacing;// Distance between chambers
     Int_t     fImedAir;
     Int_t     fImedPb;
     Int_t     fImedXe;
     
     //     TArrayI*_idMaterials;

  ClassDef(StarDetectorConstruction,1) //StarDetectorConstruction
};

#endif //STARDETECTORCONSTRUCTION_H
