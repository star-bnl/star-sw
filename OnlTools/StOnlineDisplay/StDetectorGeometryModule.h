// $Id: StDetectorGeometryModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $

#ifndef STAR_StDetectorGeometryModule
#define STAR_StDetectorGeometryModule

/*!
 *                                                                     
 * \class  StDetectorGeometryModule
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StDetectorGeometryModule virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StDetectorGeometryModule
 *
 *
 */                                                                      

#ifndef TModule_H
#include "TModule.h"
#endif
#include "StDetectorGeometryInterface.h"
#include <qstring.h>


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).

class TVolume;
class TFile;
class TVolumeView;

class StDetectorGeometryModule : public TModule, StDetectorGeometryInterface {
 private:
  // Private method declaration if any
 
 protected:
  // Protected method if any
    Int_t         mRedraw;
    TFile *fTFile; // the pointer to the actibe ROOT file
    QString fRootGeometryFileName; 
    TVolume *fTopVolume;
    TVolume *fTopOriginal;
//    TVolumeView  *fShortView;        	//!
    TList        *fListDataSetNames; 	// The list of the names to be drawn
    TList        *fVolumeList;     	   // The list of the names of TVolume object

 public: 
  StDetectorGeometryModule(const char *name="DetectorGeometry");
  virtual       ~StDetectorGeometryModule();
  virtual Int_t  Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty TModule::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty TModule::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StDetectorGeometryModule.h,v 1.1 2009/12/01 01:33:33 fine Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
public:
   // own method
   virtual void   AddVolume(const char *name);
   virtual Int_t  BuildGeometry();
   virtual void   Modified();
   virtual void   PrintVolumes();
   virtual void   RemoveVolume(const char *name);
   virtual void   SetFileName(const char* fileName);



  // ClassDef(StDetectorGeometryModule,0)   //StAF chain virtual base class for Makers
};
//_____________________________________________________________________________
inline void StDetectorGeometryModule::SetFileName(const char* fileName)
{
     fRootGeometryFileName = fileName;
}
#endif
