/*!
  \class StVertexMaker
  
  StVertexMaker finds primary vertices

*/

#ifndef STAR_StVertexMaker
#define STAR_StVertexMaker

#ifndef StMaker_H
#include "StMaker.h"
#include "TArrayF.h"
#include "TArrayI.h"
#endif

class St_evr_privert;
class St_evr_evrpar;
class dst_vertex_st;
class St_dst_track;
class St_dst_vertex;

class CtbResponse;
class MatchedTrk ;

class StVertexMaker : public StMaker {
  friend CtbResponse;
  friend MatchedTrk;
 
 private:
  St_evr_privert *m_evr_privert; //!  
  St_evr_evrpar  *m_evr_evrpar;  //!
  dst_vertex_st  *m_fixedVertex; //!
  TArrayF m_fixedArrayX;
  TArrayF m_fixedArrayY;
  TArrayF m_fixedArrayZ;
  TArrayI m_fixedArrayR;
  TArrayI m_fixedArrayE; 
  Bool_t embedVerts;

  struct {int isOn,equivNtr; float x0,y0,nx,ny;} beam4ppLMV;
  float zCutppLMV;
  float ppLMVparF[10];
  int ppLMVparI[10];
   
  TH1F *hPiFi[32];

  unsigned int mCTBMode; // 0 == DAQ, 1 == MC, 2 == Embedding

 protected:
  virtual Int_t FixVertexFileRead(char* fname, Bool_t idMatch);
  TH1F *hctb[16];
  TH1F *hmtr[16];

 public: 
  StVertexMaker(const char *name="Vertex", int key=0);
  virtual       ~StVertexMaker();
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
  long ppLMV4(MatchedTrk &, St_dst_track *trackAll,St_dst_vertex *vertex, Int_t mdate);
  void UnSetBeam4ppLMV(){beam4ppLMV.isOn=0;};
  void SetBeam4ppLMV(){
    beam4ppLMV.isOn=1; 
    printf("SetBeam4ppLMV() from DB is activated\n");
  };
  void SetBeam4ppLMV(int ntr, double x, double y, double ux, double uy){
    beam4ppLMV.equivNtr=ntr;
    beam4ppLMV.x0=x;    beam4ppLMV.nx=ux;
    beam4ppLMV.y0=y;    beam4ppLMV.ny=uy;
  } 

    void SetCTBMode(unsigned int mode){ mCTBMode = mode;};
    unsigned int GetCTBMode(){ return mCTBMode;};
    
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StVertexMaker.h,v 1.2 2002/12/04 15:43:06 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StVertexMaker, 0)   //StAF chain virtual base class for Makers
    
};
    
inline Int_t StVertexMaker::FixVertexFileEmbed(char* fname)
  { embedVerts=kTRUE; return FixVertexFileRead(fname, kTRUE); }
inline Int_t StVertexMaker::FixVertexFile3Vector(char* fname)
  { return FixVertexFileRead(fname, kFALSE); }
inline Int_t StVertexMaker::FixVertexFileMatch(char* fname)
  { embedVerts=kFALSE; return FixVertexFileRead(fname, kTRUE ); }
#endif

// $Id: StVertexMaker.h,v 1.2 2002/12/04 15:43:06 jeromel Exp $
// $Log: StVertexMaker.h,v $
// Revision 1.2  2002/12/04 15:43:06  jeromel
// Changes by J.Gans. Approved by Gene
//
// Revision 1.1  2002/02/18 21:52:59  genevb
// Introduction of StVertexMaker for finding primary vertices
//
//

