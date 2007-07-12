//////////////////////////////////////////////////////////////////////////
//                                                                      //
// DcaService                                                           //
// Author: G. Van Buren, BNL                                            //
// Tool to re-calculate DCAs for strangeMuDst classes                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "DcaService.h"
#include "StXiMuDst.hh"
#include "TMath.h"
#include "TDataMember.h"
#include "TRealData.h"
#include "phys_constants.h"
#include "math_constants.h"
#include "TClass.h"
#include "StMessMgr.h"

int GGetOffset(TClass* cl, TDataMember* that);

ClassImp(DcaService)

double DcaService::B = 0.0;
StThreeVectorD DcaService::PrimVertex(0.,0.,0.);
StThreeVectorD DcaService::Origin(0.,0.,0.);
StHelixD DcaService::Track(0.,0.,0.,DcaService::Origin,0);
int DcaService::offsetDcaXiToPrimVertex = 0;
int DcaService::offsetDcaBachelorToPrimVertex = 0;
int DcaService::offsetDcaPosToPrimVertex = 0;
int DcaService::offsetDcaNegToPrimVertex = 0;

void DcaService::initOffsets() {
  offsetDcaXiToPrimVertex = 0;
  offsetDcaBachelorToPrimVertex = 0;
  offsetDcaPosToPrimVertex = 0;
  offsetDcaNegToPrimVertex = 0;
}

void DcaService::setPrimVertex(StStrangeEvMuDst* ev) {
  PrimVertex.setX(ev->primaryVertexX());
  PrimVertex.setY(ev->primaryVertexY());
  PrimVertex.setZ(ev->primaryVertexZ());
}

double DcaService::dcaToPrimVertex(int charge, float x, float y, float z,
                                   float px, float py, float pz) {
  // Given a charge, position and momentum, calculate DCA to primary vertex
  double pt        = TMath::Sqrt(px*px + py*py);
  double bcharge   = ((double) charge)*B;
  double curvature = TMath::Abs(bcharge)*C_D_CURVATURE/pt;
  double dip       = TMath::ATan(pz/pt);
  int    h         = ((bcharge > 0) ? -1 : 1);
  double phase     = TMath::ATan2(py,px) - (h*C_PI_2);
  Origin.setX(x);
  Origin.setY(y);
  Origin.setZ(z);
  Track.setParameters(curvature, dip, phase, Origin, h);
  return Track.distance(PrimVertex);
}

double DcaService::dcaXiToPrimVertex(StXiMuDst* xi) {
  return dcaToPrimVertex(xi->charge(),
           xi->decayVertexXiX(), xi->decayVertexXiY(), xi->decayVertexXiZ(),
           xi->momXiX(),         xi->momXiY(),         xi->momXiZ());
}

double DcaService::dcaBachelorToPrimVertex(StXiMuDst* xi) {
  return dcaToPrimVertex(xi->charge(),
           xi->decayVertexXiX(), xi->decayVertexXiY(), xi->decayVertexXiZ(),
           xi->momBachelorX(),   xi->momBachelorY(),   xi->momBachelorZ());
}

double DcaService::dcaPosToPrimVertex(StV0MuDst* v0) {
  return dcaToPrimVertex(1,
           v0->decayVertexV0X(), v0->decayVertexV0Y(), v0->decayVertexV0Z(),
           v0->momPosX(),        v0->momPosY(),        v0->momPosZ());
}

double DcaService::dcaNegToPrimVertex(StV0MuDst* v0) {
  return dcaToPrimVertex(-1,
           v0->decayVertexV0X(), v0->decayVertexV0Y(), v0->decayVertexV0Z(),
           v0->momNegX(),        v0->momNegY(),        v0->momNegZ());
}

double DcaService::signIt() {
  // Give a sign of negative if the primary vertex is outside
  // the curvature of the track.
  StThreeVectorD p1 = Track.at(Track.pathLength(PrimVertex));
  StThreeVectorD p2(p1.x()-Track.xcenter(),
                    p1.y()-Track.ycenter(),0);
  StThreeVectorD p3(PrimVertex.x()-Track.xcenter(),
                    PrimVertex.y()-Track.ycenter(),0);
  if (p3.mag2() > p2.mag2()) return -1.0;
  return 1.0;
}

void DcaService::replaceDca(TObject* obj, float dca, int* offset, TClass* cl,
                            const char* memname) {
  if (!(*offset)) {
    *offset = GGetOffset(cl,cl->GetDataMember(memname));
    if (!(*offset)) LOG_WARN << Form("OFFSET NOT FOUND: %s in %s\n",
      memname,cl->GetName()) << endm;
  }
  float* cf = (float*) obj;
  float* dcaptr = (float*) ((int) cf + (*offset));
  *dcaptr = dca;
}

void DcaService::replaceDcaXiToPrimVertex(StXiMuDst* xi,float dca) {
  replaceDca(xi,dca,&offsetDcaXiToPrimVertex,xi->Class(),
    "mDcaXiToPrimVertex");
}

void DcaService::replaceDcaBachelorToPrimVertex(StXiMuDst* xi,float dca) {
  replaceDca(xi,dca,&offsetDcaBachelorToPrimVertex,xi->Class(),
    "mDcaBachelorToPrimVertex");
}

void DcaService::replaceDcaPosToPrimVertex(StV0MuDst* v0,float dca) {
  replaceDca(v0,dca,&offsetDcaPosToPrimVertex,v0->Class(),
    "mDcaPosToPrimVertex");
}

void DcaService::replaceDcaNegToPrimVertex(StV0MuDst* v0,float dca) {
  replaceDca(v0,dca,&offsetDcaNegToPrimVertex,v0->Class(),
    "mDcaNegToPrimVertex");
}

void DcaService::fixDcaXiToPrimVertex(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNXi(); j++ ) {
    fixDcaXiToPrimVertex(mk->GetXi(j));
  }
}

void DcaService::fixSignedDcaXiToPrimVertex(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNXi(); j++ ) {
    fixSignedDcaXiToPrimVertex(mk->GetXi(j));
  }
}

void DcaService::fixSignedDcaBachelorToPrimVertex(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNXi(); j++ ) {
    fixSignedDcaBachelorToPrimVertex(mk->GetXi(j));
  }
}

void DcaService::fixSignedDcaPosToPrimVertex(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNV0(); j++ ) {
    fixSignedDcaPosToPrimVertex(mk->GetV0(j));
  }
}

void DcaService::fixSignedDcaNegToPrimVertex(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNV0(); j++ ) {
    fixSignedDcaNegToPrimVertex(mk->GetV0(j));
  }
}

void DcaService::fixSignedDcasXis(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNXi(); j++ ) {
    StXiMuDst* xi = mk->GetXi(j);
    fixSignedDcaXiToPrimVertex(xi);
    fixSignedDcaBachelorToPrimVertex(xi);
    fixSignedDcaPosToPrimVertex(xi);
    fixSignedDcaNegToPrimVertex(xi);
  }
}

void DcaService::fixSignedDcasV0s(StStrangeMuDstMaker* mk) {
  initEvent(mk);
  for( Int_t j=0; j<mk->GetNV0(); j++ ) {
    StV0MuDst* v0 = mk->GetV0(j);
    fixSignedDcaPosToPrimVertex(v0);
    fixSignedDcaNegToPrimVertex(v0);
  }
}


// Providing this function separately because we should really be
// using TDataMember::GetOffset(), but it doesn't work in all versions.
// This code is copied from that used in version 1.12 of TDataMember.

int GGetOffset(TClass* cl, TDataMember* that) {
   //case of a compiled class
   //Note that the offset cannot be computed in case of an abstract class
   //for which the list of real data has not yet been computed via
   //a real daughter class.
   char dmbracket[256];
   sprintf(dmbracket,"%s[",that->GetName());
   cl->BuildRealData();
   TIter next(cl->GetListOfRealData());
   TRealData *rdm;
   while ((rdm = (TRealData*)next())) {
      char *rdmc = (char*)rdm->GetName();
      //next statement required in case a class and one of its parent class
      //have data members with the same name
      if (that->IsaPointer() && rdmc[0] == '*') rdmc++;
      
      if (rdm->GetDataMember() != that) continue;
      if (strcmp(rdmc,that->GetName()) == 0) {
         return rdm->GetThisOffset();
      }
      if (strcmp(rdm->GetName(),that->GetName()) == 0) {
         if (rdm->IsObject()) {
            return rdm->GetThisOffset();
         }
      }
      if (strstr(rdm->GetName(),dmbracket)) {
         return rdm->GetThisOffset();
      }
   }
   return 0;
}

//_____________________________________________________________________________
// $Id: DcaService.cxx,v 3.3 2007/07/12 20:01:47 fisyak Exp $
// $Log: DcaService.cxx,v $
// Revision 3.3  2007/07/12 20:01:47  fisyak
// Add includes for ROOT 5.16
//
// Revision 3.2  2007/05/19 00:35:55  genevb
// use STAR logger
//
// Revision 3.1  2002/08/13 19:18:54  genevb
// Introduction of DcaService
//
//

