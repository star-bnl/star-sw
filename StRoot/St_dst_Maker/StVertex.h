#ifndef STAR_StVertex
#define STAR_StVertex
#include "TNamed.h"
#include "St_dst_vertex_Table.h"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Event                                                                //
//                                                                      //
// Description of the event and track parameters                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class dst_vertex_st;

class StVertex : public TNamed {

private:
  Int_t          mNtrack;        // No. of secondary tracks accosiated with the vertex 
  Int_t          mType;          // Vertex type
  UInt_t         mQualityBitmask;// bitmask of e.g. quality information
  Float_t        xPosition;     
  Float_t        yPosition;
  Float_t        zPosition;

  Float_t        xPositionError;
  Float_t        yPositionError;
  Float_t        zPositionError;
  Float_t        mChiSquared;
public:
  StVertex(const Char_t *name="");
  virtual ~StVertex();
  void          AddNtrack() { mNtrack++; }
  StVertex     &Assign(dst_vertex_st &);
  void          Clear(Option_t *option ="");
  static void   Reset(Option_t *option ="");
  void          SetNtrack(Int_t n) { mNtrack = n; }
  void          SetQuality(UInt_t f) { mQualityBitmask = f; }

  Int_t         GetNtrack() const { return mNtrack; }
  UInt_t        GetQuality() const { return mQualityBitmask; }
  StVertex     &operator=(dst_vertex_st & vertex);
  ClassDef(StVertex,1)  //StVertex structure
};
#endif
