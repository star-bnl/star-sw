// $Id: StarHit.h,v 1.1 2004/07/12 20:35:58 potekhin Exp $

#ifndef STARHIT_H
#define STARHIT_H

#include <TObject.h>
#include <TVector3.h>

class StarHit : public TObject
{
  public:
    StarHit();
    virtual ~StarHit();

    // methods
    //void Draw();
    void Print(const Option_t* opt = 0) const;

    // set methods
    void SetTrackID  (Int_t    t_)  { _trackID  = t_; };
    void SetVolumeID (Int_t    v_)  { _volumeID = v_; };  
    void SetEdep     (Double_t e_)  { _edep     = e_; };
    void SetPos      (TVector3 p_)  { _pos      = p_; };
      
    // get methods
    Int_t    GetTrackID()   { return _trackID;  };
    Int_t    GetVolumeID()  { return _volumeID; };
    Double_t GetEdep()      { return _edep; };      
    TVector3 GetPos()       { return _pos; };
      
  private:
    Int_t      _trackID;
    Int_t      _volumeID;
    Double_t   _edep;
    TVector3   _pos;
    
  ClassDef(StarHit,1) //StarHit  
};

#endif //STARHIT_H


