#ifndef StiTrack_HH
#define StiTrack_HH

class StiTrack
{
 public:
  StiTrack() {};
  virtual ~StiTrack() {};
  
  void setRadius(double r) {mradius = r;}
  double radius() const {return mradius;}

 private:
  double mradius;
};
#endif
