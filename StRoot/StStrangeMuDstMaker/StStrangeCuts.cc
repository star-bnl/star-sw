/***********************************************************************
 *
 * $Id: StStrangeCuts.cc,v 3.3 2002/05/10 20:57:06 genevb Exp $
 *
 * Author: Gene Van Buren, UCLA, 26-May-2000
 *
 ***********************************************************************
 *
 * Description: handling of cuts for strangeness micro DST
 *
 ***********************************************************************
 *
 * $Log: StStrangeCuts.cc,v $
 * Revision 3.3  2002/05/10 20:57:06  genevb
 * Minor update
 *
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/01/30 04:06:45  genevb
 * Better handling of file switches
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:42  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************/
#include "TDataSet.h"
#include "TTable.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "TString.h"
#include "TClonesArray.h"
#include "StStrangeCuts.hh"
#include "StMessMgr.h"

static const TCut unknownCut("NO or UNKNOWN cuts!","");

ClassImp(StStrangeCuts)
//_____________________________________________________________________________
StStrangeCuts::StStrangeCuts() : TOrdCollection(0), update(kFALSE) {
  SetOwner();
  TCut::Class()->IgnoreTObjectStreamer();
}
//_____________________________________________________________________________
StStrangeCuts::~StStrangeCuts() {
}
//_____________________________________________________________________________
void StStrangeCuts::Fill(const char* prefix, TDataSet* cutSet) {
  // Fill cuts from tables
  TTable* cutTable = dynamic_cast<TTable*> (cutSet);
  if (!cutTable) return;
  TString colName;
  Char_t buf[16];
  Int_t nrows = cutTable->GetNRows();
  TClass* rowClass = cutTable->GetRowClass();
  for (Int_t row = 0; row<nrows; row++) {
    for (UInt_t col = 0; col<cutTable->GetNumberOfColumns(); col++) {
      const Char_t* colBaseName = cutTable->GetColumnName(col);
      ((colName = prefix) += ".") += colBaseName;
      if (nrows>1) {
        sprintf(buf,":%d",row);
        colName += buf;
      }
      UInt_t colOffset = cutTable->GetOffset(col);
      void* colValue = (void*) (((UInt_t) ((*cutTable)[0])) + colOffset);
      TDataType* colType = rowClass->GetDataMember(colBaseName)->GetDataType();
      Add(colName.Data(),colType->AsString(colValue));
    }
  }
}
//_____________________________________________________________________________
void StStrangeCuts::Append(const TOrdCollection* oldCuts) {
  // Add any new cuts to any old cuts
  if (oldCuts) {
    for (Int_t i=(oldCuts->GetSize() - 1); i>=0; i--) {
      AddIfNew((TCut*) oldCuts->At(i), kTRUE);
    }
  } else {
    gMessMgr->Warning() << "StStrangeCuts: no StrangeCuts to read in.\n "
                     << "   Creating new one." << endm;    
  }
}
//_____________________________________________________________________________
void StStrangeCuts::Reset(const TSeqCollection* oldCuts) {
  // Check to see if cuts need replaced
  Int_t cutsSize;
  if (oldCuts->IsA() == TClonesArray::Class()) {
    cutsSize = ((TClonesArray*) oldCuts)->GetEntriesFast();
  } else {
    cutsSize = oldCuts->GetSize();
  }
  Bool_t reset = (cutsSize != GetSize());
  Int_t i=0;
  while ((!reset) && (i<cutsSize)) {
    reset = NewCut((TCut*) oldCuts->At(i++));
  }
  if (reset) {
    Clear();
    for (i=0; i<cutsSize; i++) {
      Add((TCut*) oldCuts->At(i));
    }
  }
}
//_____________________________________________________________________________
Bool_t StStrangeCuts::NewCut(const TObject* aCut) {
  for (Int_t i=0; i<GetSize(); i++) {
    TObject* bCut = At(i);
    if (!(bCut->Compare(aCut))) {
      if (!(strcmp(bCut->GetTitle(),aCut->GetTitle())))
        return kFALSE;
    }
  }
  return kTRUE;
}
//_____________________________________________________________________________
void StStrangeCuts::AddIfNew(TCut* aCut, Bool_t reverse) {
  if (NewCut(aCut)) {
    if (reverse) AddFirst(aCut);
    else AddLast(aCut);
    ForceUpdateArray();
  } else {
    delete aCut;
    aCut = 0;
  }
}
//_____________________________________________________________________________
void StStrangeCuts::UpdateArray(TClonesArray* cutsArray) {
  if (update) {
    Int_t cutsSize = GetSize();
    cutsArray->Expand(cutsSize);
    for (Int_t i=0; i<cutsSize; i++) {
      new((*cutsArray)[i]) TCut(* CutAt(i));
    }
    update = kFALSE;
  }
}
//_____________________________________________________________________________
void StStrangeCuts::UnknownCuts() {
  Clear();
  Add(unknownCut);
}

