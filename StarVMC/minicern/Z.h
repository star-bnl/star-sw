#ifndef Z_H
#define Z_H

class Z_t 
{
public:
   
   Z_t* Next() const;
   Z_t* Upp () const;
   Z_t* Link(int idx) const;
   int   GetND() const;   
   int   GetNL() const;
   int   GetNS() const;
const int &GetID() const;

static void Init(void *q) { fgQ = (int*)q;}
static int  &IQ(int l);
static float &Q(int l);
static int  &LQ(int l);

static int* fgQ;
//      data
   union {
     float fDat[1];
     int   iDat[1];
   };
private:
friend class NOBODY;
   Z_t(){};
  ~Z_t(){};
};


inline int &IQ(int l){ return Z_t::fgQ[l-1];}
inline Z_t*   LQ(int l)
{ int i=Z_t::fgQ[l-1-8]; return (!i)? 0:(Z_t*)(Z_t::fgQ+i-1);}
inline float  &Q(int l){ return ((float*)Z_t::fgQ)[l-1]; }
inline Z_t* Z_t::Link(int idx) const
{ 
  int l = iDat[idx-8]; if (!l) return 0;
  return (Z_t*)(fgQ+l-1);
}


inline Z_t* Z_t::Next ()  const{ return Link(0) ;}  
inline Z_t* Z_t::Upp  ()  const{ return Link(1) ;}  
inline int  Z_t::GetND()  const{ return iDat[-1];}  
inline int  Z_t::GetNS()  const{ return iDat[-2];}  
inline int  Z_t::GetNL()  const{ return iDat[-3];}  
inline const int &Z_t::GetID()  const{ return iDat[-4];}  
#endif //Z_H
