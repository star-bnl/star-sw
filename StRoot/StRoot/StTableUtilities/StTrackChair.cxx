#include "StTrackChair.h"
#include "StHelixD.hh"
const Char_t *StTrackChair::trackTableList[] = {"tpt_track_st","dst_track_st"};

ClassImp(StTrackChair)
//_____________________________________________________________________________
static Int_t CheckName(const Char_t *key, const Char_t **values, Int_t length)
{
  int i;
  for (i=0;i<length && strcmp(key,values[i]);i++);
  if (i == length) i=-1;
  return i;
}
//_____________________________________________________________________________
StTrackChair::StTrackChair(TTable *table) : TChair(table) {
  if (!table) return;
  mR0   = table->GetOffset("r0");            //        radius at start (cm)                   
  mPhi0 = table->GetOffset("phi0");          //        azimuthal angle at start (deg)         
  mZ0   = table->GetOffset("z0");            //        z-coord. at start (cm)                 
  mPsi  = table->GetOffset("psi");           //        azimuthal angle of pT vector (deg)     
  mTanl = table->GetOffset("tanl");          //        tan(dip) =pz/pt at start               
  mCurvature = table->GetOffset("curvature");//        Track curvature (1/cm)                 
  mLength = table->GetOffset("length");      //        from first to last point (cm)          
  if (strcmp(table->GetType(),"tpt_track_st")) {
    mInvpt  = table->GetOffset("invpt") ;    //        1/pt at start (GeV/c)^(-1)             
    mCharge = table->GetOffset("icharge");   //        Particle charge in units of |e| 
  } else {
    mInvpt  = table->GetOffset("invp");      //        1/pt at start (GeV/c)^(-1)             
    mCharge = table->GetOffset("q");         //        Particle charge in units of |e| 
  }
}

//_____________________________________________________________________________
StHelixD *StTrackChair::MakeHelix(Int_t i, float bField) const
{
 //see: StEvent/StHelixModel 
  const float pi2 = 3.1415926/2.;
  const float rad = pi2/90.;
  const void *point =  At(i);
  Float_t angle   = (*(Float_t*)GetOffset(point,mPhi0)) * rad;
  int h = (*(Int_t *)GetOffset(point,mCharge))*bField > 0 ? -1 : 1;  
  Float_t ro = *(Float_t *)GetOffset(point,mR0);

  StThreeVectorD vector(ro*cos(angle),ro*sin(angle),*(Float_t *)GetOffset(point,mZ0));

  Float_t curv = *(Float_t *)GetOffset(point,mCurvature);
  Float_t tanl = *(Float_t *)GetOffset(point,mTanl);
  Float_t psi  = *(Float_t *)GetOffset(point,mPsi);

  StHelixD *helix = new  StHelixD(curv, atan(tanl), psi*rad-h*pi2, vector, h);
  return helix;
}

//_____________________________________________________________________________
Int_t StTrackChair::IsTrack(TTable *table){
   assert(table);
   return CheckName(table->GetType(),trackTableList,sizeof(trackTableList)/sizeof(Char_t *));
}
//_____________________________________________________________________________
StTrackChair *StTrackChair::Instance(TTable *table){
   assert(table);
   StTrackChair *chair = 0;
   if (CheckName(table->GetType(),trackTableList,sizeof(trackTableList)/sizeof(Char_t *))>=0) 
          chair =  new StTrackChair(table);
   return chair;
}
