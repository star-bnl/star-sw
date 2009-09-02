/*!
  \class DcaService
  \author G. Van Buren, BNL

  Tool to re-calculate DCAs for strangeMuDst classes
  \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StStrangeMuDstMaker/doc/

*/
#ifndef STAR_DcaService
#define STAR_DcaService

#include "StarClassLibrary/StHelixD.hh"
#include "StStrangeMuDstMaker.h"
#include "StStrangeEvMuDst.hh"

class StXiMuDst;
class StV0MuDst;

class DcaService {
 public:
                DcaService() {}
        virtual ~DcaService() {}

  /// @name Event initialization functions:
  //@{
  static   void setBfield(double b);
  static   void setBfield(StStrangeEvMuDst* ev);      /// B Field
  static   void setBfield(StStrangeMuDstMaker* mk);
  
  static   void setPrimVertex(StThreeVectorD& pv);
  static   void setPrimVertex(StStrangeEvMuDst* ev);  /// Primary Vertex
  static   void setPrimVertex(StStrangeMuDstMaker* mk);
  
  static   void initEvent(StStrangeEvMuDst* ev);      /// Both of the above
  static   void initEvent(StStrangeMuDstMaker* mk);
  //@}

  /// @name Functions which return recalculated DCAs
  //@{
  static double dcaXiToPrimVertex(StXiMuDst* xi);
  //   For daughter tracks:
  //     Note that the track is assumed to pass
  //     through the decay vertex, so the calc may
  //     be different from the global track calc
  static double dcaBachelorToPrimVertex(StXiMuDst* xi);
  static double dcaPosToPrimVertex(StV0MuDst* v0);
  static double dcaNegToPrimVertex(StV0MuDst* v0);
  //@}

  /// @name Functions which return signed DCAs
  //@{
  static double signedDcaXiToPrimVertex(StXiMuDst* xi);
  static double signedDcaBachelorToPrimVertex(StXiMuDst* xi);
  static double signedDcaPosToPrimVertex(StV0MuDst* v0);
  static double signedDcaNegToPrimVertex(StV0MuDst* v0);
  //@}

  /// @name Functions which replace the actual data members
  //@{
  static   void replaceDcaXiToPrimVertex(StXiMuDst* xi, Float_t dca);
  static   void replaceDcaBachelorToPrimVertex(StXiMuDst* xi, Float_t dca);
  static   void replaceDcaPosToPrimVertex(StV0MuDst* v0, Float_t dca);
  static   void replaceDcaNegToPrimVertex(StV0MuDst* v0, Float_t dca);
  //@}

  /// @name Functions which replace the data members with the correct DCAs
  //@{
  static   void fixDcaXiToPrimVertex(StXiMuDst* xi);
  static   void fixSignedDcaXiToPrimVertex(StXiMuDst* xi);
  static   void fixSignedDcaBachelorToPrimVertex(StXiMuDst* xi);
  static   void fixSignedDcaPosToPrimVertex(StV0MuDst* v0);
  static   void fixSignedDcaNegToPrimVertex(StV0MuDst* v0);
  //@}

  /// @name Functions which call the event initialization, then loop over the event and fix the DCAs
  //@{
  static   void fixDcaXiToPrimVertex(StStrangeMuDstMaker* mk);
  static   void fixSignedDcaXiToPrimVertex(StStrangeMuDstMaker* mk);
  static   void fixSignedDcaBachelorToPrimVertex(StStrangeMuDstMaker* mk);
  static   void fixSignedDcaPosToPrimVertex(StStrangeMuDstMaker* mk);
  static   void fixSignedDcaNegToPrimVertex(StStrangeMuDstMaker* mk);

  //   Multiple fixes simultaneously
  /// All DCAs for Xis
  static   void fixSignedDcasXis(StStrangeMuDstMaker* mk);
  /// All DCAs for V0s
  static   void fixSignedDcasV0s(StStrangeMuDstMaker* mk);
  /// All DCAs for V0s and Xis
  static   void fixSignedDcas(StStrangeMuDstMaker* mk);
  //@}


 private:
  static double B;
  static StThreeVectorD PrimVertex;
  static StThreeVectorD Origin;
  static StHelixD Track;
  static Long_t offsetDcaXiToPrimVertex;
  static Long_t offsetDcaBachelorToPrimVertex;
  static Long_t offsetDcaPosToPrimVertex;
  static Long_t offsetDcaNegToPrimVertex;

  static   void initOffsets();
  static double signIt();
  static   void replaceDca(TObject*, Float_t, Long_t&, TClass*, const char*);
  static double dcaToPrimVertex(int, Float_t, Float_t, Float_t, Float_t, Float_t, Float_t);

  ClassDef(DcaService,0)
};


inline void DcaService::setBfield(double b)
  { B=b; }

inline void DcaService::setBfield(StStrangeEvMuDst* ev)
  { B=ev->magneticField(); }

inline void DcaService::setBfield(StStrangeMuDstMaker* mk)
  { setBfield(mk->GetEvent()); }

inline void DcaService::setPrimVertex(StThreeVectorD& pv)
  { PrimVertex=pv; }

inline void DcaService::setPrimVertex(StStrangeMuDstMaker* mk)
  { setPrimVertex(mk->GetEvent()); }

inline void DcaService::initEvent(StStrangeEvMuDst* ev)
  { setBfield(ev); setPrimVertex(ev); }

inline void DcaService::initEvent(StStrangeMuDstMaker* mk)
  { initEvent(mk->GetEvent()); }

inline double DcaService::signedDcaXiToPrimVertex(StXiMuDst* xi)
  { return dcaXiToPrimVertex(xi)*signIt(); }

inline double DcaService::signedDcaBachelorToPrimVertex(StXiMuDst* xi)
  { return dcaBachelorToPrimVertex(xi)*signIt(); }

inline double DcaService::signedDcaPosToPrimVertex(StV0MuDst* v0)
  { return dcaPosToPrimVertex(v0)*signIt(); }

inline double DcaService::signedDcaNegToPrimVertex(StV0MuDst* v0)
  { return dcaNegToPrimVertex(v0)*signIt(); }

inline void DcaService::fixDcaXiToPrimVertex(StXiMuDst* xi)
  { replaceDcaXiToPrimVertex(xi,dcaXiToPrimVertex(xi)); }

inline void DcaService::fixSignedDcaXiToPrimVertex(StXiMuDst* xi)
  { replaceDcaXiToPrimVertex(xi,signedDcaXiToPrimVertex(xi)); }

inline void DcaService::fixSignedDcaBachelorToPrimVertex(StXiMuDst* xi)
  { replaceDcaBachelorToPrimVertex(xi,signedDcaBachelorToPrimVertex(xi)); }

inline void DcaService::fixSignedDcaPosToPrimVertex(StV0MuDst* v0)
  { replaceDcaPosToPrimVertex(v0,signedDcaPosToPrimVertex(v0)); }

inline void DcaService::fixSignedDcaNegToPrimVertex(StV0MuDst* v0)
  { replaceDcaNegToPrimVertex(v0,signedDcaNegToPrimVertex(v0)); }

inline void DcaService::fixSignedDcas(StStrangeMuDstMaker* mk)
  { fixSignedDcasXis(mk); fixSignedDcasV0s(mk); }

#endif

//_____________________________________________________________________________
// $Id: DcaService.h,v 3.3 2009/09/02 19:39:44 genevb Exp $
// $Log: DcaService.h,v $
// Revision 3.3  2009/09/02 19:39:44  genevb
// Fixes to pointer and string conversions (RT ticket 1612), prep for 64-bit
//
// Revision 3.2  2003/05/30 21:20:18  genevb
// doxygen savvy, encoding of FTPC mults, change virtual funcs
//
// Revision 3.1  2002/08/13 19:18:54  genevb
// Introduction of DcaService
//
//

