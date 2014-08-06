/*!
 * \class StV0FinderMaker
 *
 * StV0FinderMaker finds V0 secondary vertices
 *
 */

#ifndef StV0FinderMaker_hh
#define StV0FinderMaker_hh

#include "StMaker.h"
#include "StPhysicalHelixD.hh"
#include <vector>

class St_V0FinderParameters;
class V0FinderParameters_st;
class StEvent;
class StV0Vertex;
class StTrack;


//Betty
class StMuDstMaker;
//end Betty

enum TrackerUsage
{
  kTrackerUseTPT  = 0,
  kTrackerUseITTF = 1,
  kTrackerUseBOTH = 2
};



///Begin Betty
enum SVTUsage
{
  kNoSVT = 0,
  kUseSVT = 1
};

enum EventModelUsage
{
  kUseStEvent = 0,
  kUseMuDst   =1
};
///End Betty


enum V0LanguageUsage
{
  kV0LanguageUseFortran = 1,
  kV0LanguageUseCpp = 2,
  kV0LanguageUseBoth = 3
};




enum XiLanguageUsage
{
  kXiLanguageUseFortran = 1,
  kXiLanguageUseCppOnFortranV0 = 2,
  kXiLanguageUseCppOnCppV0 = 4,
  kXiLanguageUseFortranAndCppOnFortranV0 = 3,
  kXiLanguageUseFortranAndCppOnCppV0 = 5,
  kXiLanguageUseBothCpp = 6,
  kXiLanguageUseAll = 7
};




enum LanguageUsage
{
  kLanguageUseSpecial = 0,
  kLanguageUseOldRun = 1,
  kLanguageUseRun = 2,
  kLanguageUseTestV0Finder = 5,
  kLanguageUseTestXiFinder = 6,
  kLanguageUseTestBothFinders = 7
};




enum LikesignUsage
{
  kLikesignUseStandard = 0,
  kLikesignUseLikesign = 2
};




enum RotatingUsage
{
  kRotatingUseStandard = 0,
  kRotatingUseRotating = 1,
  kRotatingUseSymmetry = 2,
  kRotatingUseRotatingAndSymmetry = 3
};



/*!
For the V0Finder : all storages allowed :
   - Fortran V0s
   - C++ V0s
   - Fortran V0s and C++ V0s
For the XiFinder : list of the allowed storages :
   - if store only Fortran V0s : 
       - Fortran Xis (kLanguageUseOldRun)
       - C++ Xis made of Fortran V0s
       - Fortran Xis and C++ Xis made of Fortran V0s (kLanguageUseTestXiFinder)
   - if store only C++ V0s :
       - C++ Xis made of C++ V0s (kLanguageUseRun)
   - if store Fortran and C++ V0s :
       - Fortran Xis (kLanguageUseTestV0Finder)
       - C++ Xis made of Fortran V0s
       - Fortran Xis and C++ Xis made of Fortran V0s
       - C++ Xis made of C++ V0s
       - Fortran Xis and C++ Xis made of C++ V0s (kLanguageUseTestBothFinders)
       - C++ Xis made of Fortran V0s and C++ Xis made of C++ V0s
       - Fortran Xis, C++ Xis made of Fortran V0s and C++ Xis made of C++ V0s

For the V0Finder :
For the XiFinder :
 - kLikesignAnalysisUseLikesign : associates Lambda with pi+ and antiLambda with pi- instead of the contrary.
 - kRotatingAnalysisUseRotating : rotates all bachelor tracks by 180 degrees around the axis that is parallel to the z axis and that goes through the primary vertex.
 - kRotatingAnalysisUseSymmetry : takes the symmetric of all bachelor tracks wrt the plane that is perpandicular to the z axis and that contains the primary vertex.
 - kRotatingAnalysisUseRotatingAndSymmetry : does kRotatingAnalysisUseRotating + kRotatingAnalysisUseSymmetry (symmetry wrt the primary vertex).
*/




class StV0FinderMaker : public StMaker {

 public:
  StV0FinderMaker(const char* name="V0FinderMaker");
  virtual ~StV0FinderMaker();

  virtual void   GetPars();
  virtual Int_t  Init();
  //virtual Int_t  InitRun(int RunNumber);
  virtual Int_t  Make();
  
  //Betty
  virtual void   Clear(Option_t *option="");
  //end Betty 
  virtual void   UseExistingV0s(Bool_t opt=kTRUE) { useExistingV0s = opt; }
  virtual void   DontZapV0s(Bool_t opt=kTRUE) { dontZapV0s = opt; }
  virtual Bool_t UseV0() { return kFALSE; }
  virtual void   SetTrackerUsage(Int_t opt=0) {useTracker=opt;}
  virtual Int_t  GetTrackerUsage() {return useTracker;}
  virtual void   SetSVTUsage(Int_t opt=0) {useSVT=opt;}///Betty
  virtual Int_t  GetSVTUsage() {return useSVT;}///Betty
  virtual void   SetV0LanguageUsage(Int_t opt=0) {useV0Language=opt;}
  virtual Int_t  GetV0LanguageUsage() {return useV0Language;}
  virtual void   SetXiLanguageUsage(Int_t opt=0) {useXiLanguage=opt;}
  virtual Int_t  GetXiLanguageUsage() {return useXiLanguage;}
  virtual void   SetLanguageUsage(Int_t opt=0) {useLanguage=opt;}
  virtual Int_t  GetLanguageUsage() {return useLanguage;}
  virtual void   SetLikesignUsage(Int_t opt=0) {useLikesign=opt;}
  virtual Int_t  GetLikesignUsage() {return useLikesign;}
  virtual void   SetRotatingUsage(Int_t opt=0) {useRotating=opt;}
  virtual Int_t  GetRotatingUsage() {return useRotating;}
  virtual void   Trim();

  //Betty 
  virtual void SetEventUsage(Int_t opt=0){useEventModel=opt;}
  virtual Int_t  GetEventUsage(){return useEventModel;}
  //end Betty
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StV0FinderMaker.h,v 1.12 2014/08/06 11:43:37 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
 protected:
  virtual Int_t Prepare();         //!
  St_V0FinderParameters* v0pars;   //!
  V0FinderParameters_st* pars;     //!
  V0FinderParameters_st* pars2;    //!
  StEvent* event;                  //!
  StV0Vertex* v0Vertex;            //!
  
   //Betty
  StMuDstMaker* mMuDstMaker;       //!
  //end Betty
  
  double Bfield;		   //!
  unsigned short trks;		   //!
  Bool_t prepared;		   //!
  Bool_t useExistingV0s;           //!
  Bool_t dontZapV0s;		   //!
  Int_t useTracker;		   //!
  Int_t useSVT;        //Betty	   //!
  Int_t useEventModel; //Betty     //!
  Int_t useV0Language;		   //!
  Int_t useXiLanguage;		   //!
  Int_t useLanguage;               //!
  Int_t useLikesign;		   //!
  Int_t useRotating;		   //!
  int det_id_v0;		   //!
  int ITTFflag;		           //!
  int TPTflag;                     //!
  
  vector<StTrack*> trk;            //!
  vector<unsigned short> ntrk;     //!
  vector<unsigned short> ptrk;     //!
  vector<short> hits;              //!
  vector<int> detId;               //!
  vector<double> pt;               //!
  vector<double> ptot;             //!
  vector<StPhysicalHelixD> heli;   //!
  vector<unsigned short> trkID;    //!
  void ExpandVectors(unsigned short size);

  StThreeVectorD mainv;            //!

 private:
  static StV0FinderMaker* mInstance;      //!
  int trkcnt;
  int trkmax;
  float trkNodeRatio;
  float trkNodeRatioCnt;

  ClassDef(StV0FinderMaker,0)

};

#endif

//_____________________________________________________________________________
// $Id: StV0FinderMaker.h,v 1.12 2014/08/06 11:43:37 jeromel Exp $
// $Log: StV0FinderMaker.h,v $
// Revision 1.12  2014/08/06 11:43:37  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.11  2008/03/05 04:20:18  genevb
// Change to DB table of V0FinderParameters, reduce logger output, improve Bfield calc
//
// Revision 1.10  2004/08/26 03:00:46  genevb
// Improved vector size management
//
// Revision 1.9  2004/08/11 21:26:38  genevb
// Trade static arrays for vectors
//
// Revision 1.8  2004/04/02 08:56:22  faivre
// Use actual TPT flag rather than "not ITTF" for TPT tracks.
//
// Revision 1.7  2004/03/03 01:46:13  cmironov
// (J.L. commit)
// Modified doxygen documentation (was not taken before). Indentation
// for numerous enum{} which would need some //< comments for usage
// summary ...
//
// Revision 1.6  2004/01/27 17:56:06  betya
//
// added EventModelUsage so that the V0Finder and XiFinder can no run on
// MuDst as well as on StEvent.  Note that the output is still in the StEvent
// format.  Added Clear() in StV0FinderMaker.cxx to accomodate this addition.
//
// Revision 1.5  2003/11/08 18:25:48  faivre
// Bfield + consistency int/short
//
// Revision 1.4  2003/08/22 17:47:14  caines
// Get sign AND magnitude of mag field correctly for Xi and V0 finder
//
// Revision 1.3  2003/05/14 19:14:41  faivre
// Setting new enum's. Fancy choices Fortran/C++ V0's and Xi's. Xi rotating and like-sign. SVT tracks.
//
// Revision 1.2  2003/04/30 19:13:52  faivre
// ITTF vs TPT V0s
//
//
