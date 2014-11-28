/*!
 * \class StStrangeCuts
 * \author Gene Van Buren, UCLA, 26-May-2000
 *
 *              Handling of cuts for strangeness micro DST
 * \sa http://www.star.bnl.gov/STAR/comp/pkg/dev/StRoot/StStrangeMuDstMaker/doc/
 *
 */

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

  /// @name Accessor functions
  //@{
  TCut* CutAt(Int_t idx) const;
  TCut* GetCut(const char* name) const;
  const char* GetValue(const char* name) const;
  void List();
  Bool_t Contains(const TObject*);
  //@}

  /// @name Add cut functions
  //@{
  void Add(const char* name, const char* value);
  void Add(const TCut&);
  void Add(const TCut*);
  void Add(TObject *to){TSeqCollection::Add(to);}
  //@}

  /// @name Functions for StStrangeMuDstMaker operations
  //@{
  void Store();
  void Fill(const char*, TDataSet*);
  void Append(const TOrdCollection*);
  void Reset(const TSeqCollection&);
  void Reset(const TSeqCollection*);
  void UpdateArray(TClonesArray*);
  void ForceUpdateArray();
  void UnknownCuts();
  void Init();
  //@}

 protected:
  void AddIfNew(TCut*, Bool_t reverse=kFALSE);
  Bool_t NewCut(const TObject*);
  Bool_t update;
  StStrangeCuts* additionals;
  StStrangeCuts* lastResetCuts;
  ClassDef(StStrangeCuts,0)
};

inline TCut* StStrangeCuts::CutAt(Int_t idx) const
            { return (TCut*) At(idx); }
inline TCut* StStrangeCuts::GetCut(const char* name) const
            { return (TCut*) FindObject(name); }
inline const char* StStrangeCuts::GetValue(const char* name) const
            { return GetCut(name)->GetTitle(); }
inline void StStrangeCuts::Add(const char* name, const char* value)
            { AddIfNew(new TCut(name,value)); }
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
inline Bool_t StStrangeCuts::NewCut(const TObject* obj)
            { return (!(Contains(obj))); }

#endif


/***********************************************************************
 * $Id: StStrangeCuts.hh,v 3.5 2003/09/07 03:49:05 perev Exp $
 * $Log: StStrangeCuts.hh,v $
 * Revision 3.5  2003/09/07 03:49:05  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 3.4  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.3  2003/02/10 15:59:20  genevb
 * Fix bug with adding new cuts
 *
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
 ***********************************************************************/
