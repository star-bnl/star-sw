#ifndef STAR_StPrimaryMaker
#define STAR_StPrimaryMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPrimaryMaker virtual base class for Maker                          //
//                                                                      //
// $Id: StPrimaryMaker.h,v 1.13 2001/11/28 23:51:45 balewski Exp $
// $Log: StPrimaryMaker.h,v $
// Revision 1.13  2001/11/28 23:51:45  balewski
// *** empty log message ***
//
// Revision 1.12  2001/11/28 23:02:58  balewski
// ppLMV uses only tracks matched to CTB slats
//
// Revision 1.11  2001/09/07 23:18:16  genevb
// Additional vertex fixing from file capabilities
//
// Revision 1.10  2001/09/05 21:28:29  genevb
// Added vertex file-reading ability
//
// Revision 1.9  2001/06/12 22:59:38  balewski
// reject pileup in ppLMV
//
// Revision 1.8  2001/04/12 15:46:27  balewski
// *** empty log message ***
//
// Revision 1.7  1999/12/10 17:38:42  genevb
// Added fixed vtx functionality, allow lmv and fixed vtx only one vtx entry
//
// Revision 1.6  1999/09/12 23:03:04  fisyak
// Move parameters into makers
//
// Revision 1.5  1999/07/15 13:57:53  perev
// cleanup
//
// Revision 1.4  1999/07/12 23:04:16  fisyak
// Remove glob2
//
// Revision 1.3  1999/07/08 19:09:52  fisyak
// Add tabs, remove St_glb_Maker
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#include "TArrayF.h"
#include "TArrayI.h"
#endif

class St_evr_privert;
class St_evr_evrpar;
class St_egr_propagate;
class St_egr_egrpar;

class St_svg_shape; 
class St_svg_config; 
class St_svg_geom ;
class St_srs_activea;
class St_srs_srspar;
class dst_vertex_st;
class St_dst_track;
class St_dst_vertex;

class CtbResponse;
class MatchedTrk ;

class StPrimaryMaker : public StMaker {
  friend CtbResponse;
  friend MatchedTrk;
 
 private:
  Int_t            m_flag;       //
  St_evr_privert *m_evr_privert; //!  
  St_evr_evrpar  *m_evr_evrpar;  //!
  St_egr_propagate *m_tp_param;  //!
  St_egr_egrpar  *m_egr_egrpar;  //!
  St_egr_egrpar  *m_egr2_egrpar; //!
  dst_vertex_st  *m_fixedVertex; //!
  TArrayF m_fixedArrayX;
  TArrayF m_fixedArrayY;
  TArrayF m_fixedArrayZ;
  TArrayI m_fixedArrayR;
  TArrayI m_fixedArrayE;
  Bool_t embedVerts;
  float zCutppLMV;
  float ppLMVparF[10];
  int ppLMVparI[10];
  long ppLMV4(MatchedTrk &, St_dst_track *trackAll,St_dst_vertex *vertex, Int_t mdate);
  TH1F *hPiFi[16];
 protected:
  virtual Int_t FixVertexFileRead(char* fname, Bool_t idMatch);
  TH1F *hctb[16];
  TH1F *hmtr[16];

 public: 
  StPrimaryMaker(const char *name="primary");
  virtual       ~StPrimaryMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void  FixVertex(Float_t x=0, Float_t y=0, Float_t z=0);
  virtual Int_t FixVertex(Int_t eventNumber);
  virtual Int_t FixVertex(Int_t runID, Int_t eventID);
  virtual Int_t FixVertexFileEmbed(char* fname);
  virtual Int_t FixVertexFile3Vector(char* fname);
  virtual Int_t FixVertexFileMatch(char* fname);
  virtual void  UnFixVertex();
  Int_t GetFixedSize() { return m_fixedArrayX.GetSize(); }
  Int_t GetMatchedSize() { return m_fixedArrayE.GetSize(); }
  void ppLMVuse(int *parI, float *parF);
  void ppLMVuse(float z); // obsolete JB
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPrimaryMaker.h,v 1.13 2001/11/28 23:51:45 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StPrimaryMaker, 0)   //StAF chain virtual base class for Makers
    };
    
inline Int_t StPrimaryMaker::FixVertexFileEmbed(char* fname)
  { embedVerts=kTRUE; return FixVertexFileRead(fname, kTRUE); }
inline Int_t StPrimaryMaker::FixVertexFile3Vector(char* fname)
  { return FixVertexFileRead(fname, kFALSE); }
inline Int_t StPrimaryMaker::FixVertexFileMatch(char* fname)
  { embedVerts=kFALSE; return FixVertexFileRead(fname, kTRUE ); }
#endif
    
