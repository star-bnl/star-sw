#ifndef STARVOLUME_H
#define STARVOLUME_H

/* $Id: StarVolume.h,v 1.3 2004/09/03 00:03:33 potekhin Exp $ */

////////////////////////////////////////////////////////
//                                                    //
//                                                    //
////////////////////////////////////////////////////////


#include <TNamed.h>
#include <THashTable.h>

// 
class StarVolume: public TNamed {

 public:
  StarVolume();
  StarVolume(const char* name_, const char *title_);
  StarVolume(const StarVolume &vol_);
  virtual ~StarVolume();

  int    GetNumber(void)    const {return _number;}
  void   SetNumber(Int_t n_)      {_number=n_;}
  Int_t  IsSensitive(void) const  {return  _sensitive;}
  void   SetSensitive(Int_t s_)   {_sensitive=s_;}


  static StarVolume*      GetInstance() {return 0;} // dummy

  static Int_t            GetNextNumber(void)    {return ++_nVolumes;}
  static StarVolume*      FindVolume(const char* name_);
  static StarVolume*      FindVolume(      Int_t numb_);

  static void             Volume(const char* name_,   const char* shape_,  const char* medium_,
				 Float_t *upar_, Int_t npar_);

  static void             Division(const char* nameDiv_,   const char* nameMother_, Int_t howMany_, Int_t axis_);


 protected:

 private:
  Int_t         _number;
  Int_t         _sensitive;  

  static Int_t  _nVolumes;
  static TList  _volumes;

  THashTable*   _lkp;

  ClassDef(StarVolume,0)
    };
#endif //STARVOLUME_H
