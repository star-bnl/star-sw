#ifndef StKinkTrkIdCheck_hh
#define StKinkTrkIdCheck_hh

#if !defined(ST_NO_NAMESPACES) 
using namespace std;
#endif

#include "TObject.h"

class StKinkTrkIdCheck:public TObject {
public:
  StKinkTrkIdCheck();
  ~StKinkTrkIdCheck();
  // StKinkTrkIdCheck(const StKinkTrkIdCheck&);                  use default
  // const StKinkTrkIdCheck& operator=(const StKinkTrkIdCheck&); use default
  
  // Set mCommonIdp to 1 if two parents have common dst_track id
  void setCommonIdp(Int_t val); 
  // Set mCommonIdd to 1 if two daughters have common dst_track id 
  void setCommonIdd(Int_t val);
  void setPosInKinkVtx(Int_t val);

  Int_t commonIdp();    
  Int_t commonIdd();
  Int_t posInKinkVtx();

private:
  Int_t mCommonIdp; 
  Int_t mCommonIdd;  
  Int_t mPosInKinkVtx; // Position in dst_tkf_vertex table
};

 inline Int_t StKinkTrkIdCheck::commonIdp() { return mCommonIdp; } 
 inline Int_t StKinkTrkIdCheck::commonIdd() { return mCommonIdd; }
 inline Int_t StKinkTrkIdCheck::posInKinkVtx() { return mPosInKinkVtx; }

#endif
