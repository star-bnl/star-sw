//StiHit.h
//M.L. Miller (Yale Software)
//04/01

//Hit class to be used for the ITTF tracker

#ifndef StiHit_HH
#define StiHit_HH

class ostream;

class StiHit
{
 public:
  StiHit() {};

  StiHit(const StiHit& rhs) {
    if (*this==rhs) return;
    copyToThis(rhs);
    return;
  }
  
  ~StiHit() {};
  
  inline double x() const {return mx;}
  inline double y() const {return my;}
  inline double z() const {return mz;}
  inline unsigned int sector() const {return msector;}
  inline unsigned int padrow() const {return mpadrow;}
  
  inline void setX(double val) {mx=val;}
  inline void setY(double val) {my=val;}
  inline void setZ(double val) {mz=val;}
  inline void setSector(unsigned int val) {msector=val;}
  inline void setPadrow(unsigned int val) {mpadrow=val;}
  
  inline bool operator==(const StiHit& rhs) {
    return ( msector==rhs.msector && mpadrow==rhs.mpadrow &&
	     mx==rhs.mx && my==rhs.my && mz==rhs.mz );
  }

  inline StiHit& operator=(const StiHit& rhs) {
    if (*this==rhs) return *this;
    copyToThis(rhs);
    return *this;
  }
  
 private:
  
  void copyToThis(const StiHit& rhs) {
    msector = rhs.msector;
    mpadrow = rhs.mpadrow;
    mx = rhs.mx;
    my = rhs.my;
    mz = rhs.mz;
  }
  
  unsigned int msector;
  unsigned int mpadrow;
  double mx;
  double my;
  double mz;

};

//Non-member functions

ostream& operator<<(ostream&, const StiHit&);

#endif
