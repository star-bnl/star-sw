/***********************************************************************
 *
 * $Id: StStrangeCuts.hh,v 2.0 2000/06/05 05:19:43 genevb Exp $
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

class StStrangeCuts {
public:
  StStrangeCuts();
  virtual ~StStrangeCuts();
  TOrdCollection* GetCollection();
  TCut* GetCut(const char* name) const;
  const char* GetValue(const char* name) const;
  void Add(const char* name, const char* val);
  void Add(TCut&);
  void List();
  void Store();
  void Fill(const char*, TDataSet*);
  void Append(TOrdCollection*);
  void Assure();
 protected:
  TOrdCollection* cuts;
  ClassDef(StStrangeCuts,0)
};

inline TOrdCollection* StStrangeCuts::GetCollection()
            { return cuts; }
inline TCut* StStrangeCuts::GetCut(const char* name) const
            { return (cuts ? (TCut*) cuts->FindObject(name) : 0); }
inline const char* StStrangeCuts::GetValue(const char* name) const
            { return (cuts ? GetCut(name)->GetTitle() : 0); }
inline void StStrangeCuts::Add(const char* name, const char* val)
            { Assure(); cuts->AddLast(new TCut(name,val)); }
inline void StStrangeCuts::Add(TCut& newCut)
            { Assure(); cuts->AddLast(new TCut(newCut)); }
inline void StStrangeCuts::Assure()
            { if (!cuts) cuts = new TOrdCollection(); }
inline void StStrangeCuts::List()
            { if (cuts) cuts->Print(); }
inline void StStrangeCuts::Store()
            { if (cuts) cuts->Write("StrangeCuts",TObject::kSingleKey); }

#endif
