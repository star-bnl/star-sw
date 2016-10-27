// $Id: St_geant_Maker.h,v 1.55 2014/08/06 11:43:54 jeromel Exp $
// $Log: St_geant_Maker.h,v $
// Revision 1.55  2014/08/06 11:43:54  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.54  2012/11/26 18:45:55  jwebb
// Restoring to previous version of St_geant_Maker and adding in changes needed
// for new generator framework (i.e. exposing TGiant3 instance).
//
// Revision 1.52  2012/11/14 00:02:12  fisyak
// Add flux histograms, use Attributes intead of m_Mode
//
// Revision 1.51  2011/09/11 20:57:14  fisyak
// Add kinematics definition via MuDst, Clean up
//
// Revision 1.50  2010/08/10 16:35:33  fisyak
// Add initialization of starsim parameter tables after opening zebra-file
//
// Revision 1.49  2010/05/27 13:36:14  fisyak
// 3rd attemp to synchronize mag.field. Now take care that the maker can be not Active and do InitRun in Work
//
// Revision 1.47  2010/05/24 15:38:40  fisyak
// Move geometry and mag.field initialization from Init into InitRun in order to allow mag. field settings from StMagFMaker::InitRun
//
// Revision 1.45  2010/05/10 14:19:52  fisyak
// move geometry load from Init to InitRun in order to allow MagF maker to set mag.field
//
// Revision 1.44  2009/12/31 00:02:59  perev
// Add the material name to the volume name
//
// Revision 1.43  2008/07/30 15:04:36  fisyak
// Remove custom SetDebug, fix bug #1252
//
// Revision 1.42  2007/07/12 20:36:03  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.41  2007/07/12 20:35:30  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.40  2007/04/26 15:51:31  fisyak
// Move creation of TGiant3 in ctor (fix byg 942)
//
// Revision 1.39  2007/03/03 00:35:43  fine
// Fix the leak of the ROOT objects and introduce the method to return the source code filename  for the arbitrary geometry node
//
// Revision 1.38  2005/11/22 23:13:24  fisyak
// Add default kinematics if there is no input fiels and if maker is active
//
// Revision 1.37  2005/10/06 19:23:07  fisyak
// Add set date/time from fz-file
//
// Revision 1.36  2005/08/29 21:47:09  fisyak
// Changes for VMC
//
// Revision 1.35  2005/04/13 22:27:11  fisyak
// Add Hit description extractor (AgstHits)
//
// Revision 1.34  2005/02/07 21:09:20  fisyak
// rename antique TGeant3 to TGiant3
//
// Revision 1.33  2004/02/10 23:16:34  potekhin
// First version of Ag2Geom
//
// Revision 1.32  2003/10/01 23:54:08  potekhin
// Added a declaration a a pointer to the structure geom_gdat,
// needed for the propagation the GEANT run data --
// geometry tag and field scale.
//
// Revision 1.31  2003/09/10 19:47:47  perev
// ansi corrs
//
// Revision 1.30  2002/11/01 03:17:42  fine
// the previous version has been restored. No need of the special flag
//
// Revision 1.28  2002/03/12 21:22:39  fisyak
// Set only one StEvtHddr as default option (due to Embedding)
//
// Revision 1.27  2001/06/01 03:03:57  perev
// overloaded GetDataSet -> FindDataSet
//
// Revision 1.26  2001/05/31 16:06:12  perev
// hiding
//
// Revision 1.25  2000/09/23 03:07:13  fine
// class TShape; statement introduced to fix a side aeffect of TDataSet clean up
//
// Revision 1.24  2000/03/26 02:43:22  fine
//  adjusted to ROOT 2.24
//
// Revision 1.23  2000/02/07 18:58:38  fisyak
// Set default NwGeant = 20 000 000 words
//
// Revision 1.22  2000/02/03 19:34:42  fisyak
// Clean up St_geant_Maker::Init, move its parameters to ctor
//
// Revision 1.21  2000/02/03 16:15:54  fisyak
// Add Kathy's histograms
//

#ifndef STAR_St_geant_Maker
#define STAR_St_geant_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geant_Maker virtual base class for Maker                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StMaker.h"
//#define DetectorIndex
#ifdef DetectorIndex
#include "TArrayI.h"
#endif
#include "TGiant3.h"
class TVolume;
class TRotMatrix;
class TShape;
class TGeoVolume;
class St_geom_gdat;
class TFileSet;
class St_geant_Maker : public StMaker {
protected:
  Int_t  fNwGeant;     // No. of words in GCBANK common block
  Int_t  fNwPaw;       // No. of words in PAWC  common block
  Int_t  fIwType;      // HIGZ interface (=0 no HIGZ)
  TDataSet*   fVolume; //!
  TGeoVolume* fTopGeoVolume; //!
  TString fInputFile; // 
  TFileSet  *fGeoDirectory; // the pointer the STAR geometry source code
  StEvtHddr *fEvtHddr;//! pointer to Event Header
  virtual TShape  *MakeShape(TString *name, Int_t ivo);
  virtual TVolume *MakeVolume(TString *name, Int_t ivo, Int_t Nlevel, Int_t *Names, Int_t *Numbers);
  virtual void ClearRootGeoms();
 private:

  St_geom_gdat *m_geom_gdat;
 public: 
                  St_geant_Maker(const char *name="geant",
				 Int_t nwgeant=20,Int_t nwpaw=0, Int_t iwtype=0);
   virtual       ~St_geant_Maker(){};
   virtual Int_t  Finish(){SafeDelete(m_DataSet); return kStOK;}
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t run);
   virtual void   SetDateTime(int idat=0,int itim=0);//
           void   SetFieldOpt(const char *opt) {mFieldOpt = opt;}
   /// Executes a KUIP command
   virtual void   Do(const Char_t *option = "dcut cave x 0.1 10 10 0.03 0.03"); // *MENU 
   virtual void   Draw(const char* opt="IN");
   virtual Int_t  Make();
   /// Specifies GEANT3 geometry command
   virtual void   LoadGeometry (const Char_t *option = "detp geometry field_only");  // *MENU
   virtual void   SetNwGEANT (Int_t n=2) {fNwGeant = n;}
   virtual void   SetNwPAW   (Int_t n=0) {fNwPaw   = n;}
   virtual void   SetIwtype  (Int_t n=0) {fIwType  = n;}
   virtual Int_t  GetNwGEANT () {return fNwGeant;}
   virtual Int_t  GetNwPAW   () {return fNwPaw  ;}
   virtual Int_t  GetIwtype  () {return fIwType ;}
   virtual Int_t  Skip(Int_t Nskip=1);                        // *MENU*
   virtual TDataSet *Work();
   virtual void   Mark(TVolume *topvol);
   virtual void   Call(const Char_t *name); // *MENU 
   virtual TRotMatrix *GetMatrix(float theta1, float phi1,
                                 float theta2, float phi2,
                                 float theta3, float phi3);


   Int_t SetInputFile(const char* file);

   TDataSet* GetVolume() { return fVolume; }
   const TFileSet *GetGeoDirectory() const { return fGeoDirectory;}
   TGeoVolume* GetTopGeoVolume() {return fTopGeoVolume;}
   static void RootMapTable(Char_t *Cdest,Char_t *Table, Char_t* Spec, 
			    Int_t &k, Char_t *iq);
   virtual void     Geometry();
   virtual Int_t    Agstroot();
   virtual Int_t    AgstHits();
   virtual Int_t    G2t_volume_id(const Char_t *name, Int_t *numbv);
   virtual Int_t    Agvolume(TVolume *&node,Float_t *&par,Float_t *&pos,Float_t *&mot,
   			     Int_t &who, Int_t &copy,Float_t *&par1,Int_t &npar, char mat[21]);
   virtual void     Agnzgete (Int_t &ILK, Int_t &IDE,
			      Int_t &NPART, Int_t &IRUN,
			      Int_t &IEVT, const Char_t *CGNAM,
			      Float_t *VERT,Int_t &IWTFL,Float_t &WEIGH);
   
   virtual void     Gfxzrm(Int_t & Nlevel, 
		     Float_t &x, Float_t &y, Float_t &z,
		     Float_t &Theta1, Float_t & Phi1,
		     Float_t &Theta2, Float_t & Phi2,
		     Float_t &Theta3, Float_t & Phi3,
		     Float_t &Type);  
   virtual void     Dzddiv(Int_t& idiv ,Int_t &Ldummy,
			   const Char_t* path,const Char_t* opt,
			   Int_t& one,Int_t &two,Int_t &three,Int_t& iw);
#ifdef DetectorIndex
  void        DetSetIndex();
  void        DumpIndex(const Char_t *name, const Char_t *vers, const Char_t *fmt, TArrayI &NVmax, TArrayI &Ids);
#endif
  TString GetVolumeSrcFile(const char *volumeName) const;
  Int_t   KinematicsFromMuDst(Int_t flag=0);
  Int_t   SetDatimeFromMuDst();
  static St_geant_Maker *instance() {return fgGeantMk;}
  static void usflux();
  static Int_t ipartx(Int_t id);
  static Float_t dose(Float_t Z);
  static Bool_t GeometryVersion();
  static void Version(ostream& os);
  /// Returns a pointer to the GEANT3 VMC interface
  TGiant3 *Geant3(){ return geant3; }
 protected:
   virtual TDataSet  *FindDataSet (const char* logInput,
                                    const StMaker *uppMk=0,
                                    const StMaker *dowMk=0) const ;
   static TDataSet   *fgGeom; //!
   static TGiant3    *geant3; //!
   static St_geant_Maker *fgGeantMk; //!
   TString           mInitialization; // !
   TString           mFieldOpt; // !
   static Quest_t   *cquest; //! 
   static Gclink_t  *clink; //! 
   static Gcflag_t  *cflag; //! 
   static Gcvolu_t  *cvolu; //! 
   static Gcnum_t   *cnum; //! 
   static Int_t     *z_iq, *z_lq; //! 
   static Float_t   *z_q; //! 
   static Gcsets_t  *csets; //!
   static Gckine_t  *ckine; //!
   static Gcking_t  *cking; //!
   static Gctrak_t  *ctrak; //!
   static Gcmate_t  *cmate; //!
   static Gccuts_t  *ccuts; //!
   static Gcphys_t  *cphys; //!
   static Int_t      nlev;  //!
   static Bool_t     fInitRunDone; //!
   static Kine_gdat_t *kine_gdat;  //!
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: St_geant_Maker.h,v 1.55 2014/08/06 11:43:54 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(St_geant_Maker,0)   //StAF chain virtual base class for Makers
};

#endif

