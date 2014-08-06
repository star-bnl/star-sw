/***********************************************************************
 *
 * $Id: StStrangeCuts.hh,v 3.2 2002/04/30 16:02:47 genevb Exp $
 *
 * Author: Gene Van Buren, UCLA, 26-May-2000
 *
 ***********************************************************************
 *
 * Description: handling of cuts for strangeness micro DST
 *
 ***********************************************************************
 *
 * $Log: StStrangeCuts.hh,v $
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/01/30 04:06:54  genevb
 * Better handling of file switches
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:43  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************/
#ifndef StStrangeCuts_hh
#define StStrangeCuts_hh
#include "TOrdCollection.h"
#include "TCut.h"
#include "TObject.h"

class TDataSet;
class TClonesArray;

class StStrangeCuts : public TOrdCollection {
public:
  StStrangeCuts();
  virtual ~StStrangeCuts();
  TCut* CutAt(Int_t idx) const;
  TCut* GetCut(const char* name) const;
  const char* GetValue(const char* name) const;
  void Add(const char* name, const char* val);
  void Add(const TCut&);
  void Add(const TCut*);
  void List();
  void Store();
  void Fill(const char*, TDataSet*);
  void Append(const TOrdCollection*);
  void Reset(const TSeqCollection&);
  void Reset(const TSeqCollection*);
  void UpdateArray(TClonesArray*);
  void ForceUpdateArray();
  void UnknownCuts();
 protected:
  void AddIfNew(TCut*, Bool_t reverse=kFALSE);
  Bool_t NewCut(const TObject*);
  Bool_t update;
  ClassDef(StStrangeCuts,0)
};

inline TCut* StStrangeCuts::CutAt(Int_t idx) const
            { return (TCut*) At(idx); }
inline TCut* StStrangeCuts::GetCut(const char* name) const
            { return (TCut*) FindObject(name); }
inline const char* StStrangeCuts::GetValue(const char* name) const
            { return GetCut(name)->GetTitle(); }
inline void StStrangeCuts::Add(const char* name, const char* val)
            { AddIfNew(new TCut(name,val)); }
inline void StStrangeCuts::Add(const TCut& newCut)
            { AddIfNew(new TCut(newCut)); }
inline void StStrangeCuts::Add(const TCut* newCut)
            { Add(*newCut); }
inline void StStrangeCuts::List()
            { Print(); }
inline void StStrangeCuts::Store()
            { Write("StrangeCuts",TObject::kSingleKey); }
inline void StStrangeCuts::Reset(const TSeqCollection& oldCuts)
	    { Reset(&oldCuts); }
inline void StStrangeCuts::ForceUpdateArray()
	    { update = kTRUE; }

#endif
