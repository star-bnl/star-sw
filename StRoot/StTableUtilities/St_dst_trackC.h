#include "TChair.h"
#include "tables/St_dst_track_Table.h"

class St_dst_trackC : public TChair
{
 protected:
    Double_t       mMomentum[3];
 public:
    St_dst_trackC ();
    St_dst_trackC (St_dst_track *table);
    Double_t AbsMoment(Int_t i=-1);
    const Double_t *momentum(Int_t i=-1);
    Int_t GetCurrentIndex(){return fLastIndx;}
    ClassDefChair(St_dst_track, dst_track_st )
    ClassDef(St_dst_trackC,1) //C++ TChair for dst_track table class
};

//_____________________________________________________________________________
inline Double_t St_dst_trackC::AbsMoment(Int_t i){
   momentum(i); 
   return  ::sqrt(mMomentum[0]*mMomentum[0]+mMomentum[1]*mMomentum[1]+mMomentum[2]*mMomentum[2]);
}
