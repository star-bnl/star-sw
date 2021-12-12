#ifndef __StMuMcVertex_h__
#define __StMuMcVertex_h__
#include "tables/St_g2t_vertex_Table.h" 
#include "StThreeVectorF.hh"

class StMuMcVertex : public TObject {
 public:
#ifndef __TFG__VERSION__
  StMuMcVertex() {}
#else /* __TFG__VERSION__ */
  StMuMcVertex();
#endif /* ! __TFG__VERSION__ */
  StMuMcVertex(const g2t_vertex_st &v) : mId(v.id), mNoDaughters(v.n_daughter), mIdParTrk(v.parent_p), 
    mIsInterm(v.is_itrmd), mTime(v.ge_tof), mXyzV(v.ge_x) {}
  virtual ~StMuMcVertex() {}
  Int_t Id()                   const {return mId;}          /* primary key */			
  Int_t NoDaughters()          const {return mNoDaughters;} /* Number of daughter tracks */	
  Int_t IsIntermedate()        const {return mIsInterm;}    /* flags intermediate vertex */	
  Float_t Time()               const {return mTime;}        /* GEANT vertex production time (nsec) */
  const StThreeVectorF &XyzV() const {return *&mXyzV;}      /* GEANT vertex coordinate (Cartesian) */
  Int_t IdParTrk()             const {return mIdParTrk;}
  virtual void Print(Option_t* option = "") const;          //
 private:
#ifdef __TFG__VERSION__
  
  Char_t         mBeg[1];      //!
#endif /* __TFG__VERSION__ */
  Int_t   mId;
  Int_t   mNoDaughters;
  Int_t   mIdParTrk;
  Int_t   mIsInterm;                    
  Float_t mTime;
#ifdef __TFG__VERSION__
  Char_t         mEnd[1];      //!
#endif /* __TFG__VERSION__ */
  StThreeVectorF mXyzV;
#ifndef __TFG__VERSION__
  ClassDef(StMuMcVertex,1)
#else /* __TFG__VERSION__ */
  ClassDef(StMuMcVertex,2)
#endif /* __TFG__VERSION__ */
};
ostream&              operator<<(ostream& os, StMuMcVertex const & v);
#endif
