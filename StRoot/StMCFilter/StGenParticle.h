// @(#)STAR/eg:$Id: StGenParticle.h,v 1.4 2009/05/09 00:44:58 perev Exp $
// Author: V.Perev  Mar/2009
////////////////////////////////
//                                                                      //
// StGenParticle: defines  equivalent of HEPEVT particle                //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_StGenParticle
#define ROOT_StGenParticle


/// Abstract base class for particles related to common /HEPEVT/
class StGenParticle  {

protected:
                                // ****** constructors and destructor
   StGenParticle(int idx=0){mIdx=idx;}

   virtual ~StGenParticle(){;}

void SetIdx(int idx) { mIdx =  idx;}
int  GetIdx() const  { return mIdx;}

public:

virtual void                 Print(const char *opt="") const;
virtual int          	     GetStatusCode()     const=0;
virtual int          	     GetPdgCode()        const;
virtual int          	     GetGeaCode()        const;
virtual const StGenParticle *GetMother(int i=0)  const=0; 
virtual const StGenParticle *GetDaughter(int i)  const=0; 
virtual       double         GetCalcMass     ()  const;
virtual       double         GetMass         ()  const=0;
virtual       int            GetNDaughters   ()  const=0;
virtual       double         GetWeight       ()  const { return 1.;}
virtual       void           Momentum(double p4[4])   const=0 ;
virtual       void           Vertex(double v[3]) const=0;
virtual       double         Time() const=0;
              int            IsPrimary       ()  const { return !GetMother(0);} //Is this particle primary one?
              int            IsFinal         ()  const { return !GetStatusCode()==1;} //Is this particle final one?

double        R     () const;
double        Rho   () const;
double        P     () const;
double        Pt    () const;
double        Energy() const;
double        Eta   () const;
double        Phi   () const;
double        Theta () const;

protected:

  int mIdx;
};

class StGenParticleMaster  {


protected:
                                // ****** constructors and destructor
   StGenParticleMaster(){;}

public:
int Size() const { return mNTk;}

virtual ~StGenParticleMaster(){;}
virtual void Update()=0;
virtual void Print(const char *tit) const;

virtual const StGenParticle *operator()(int idx) const=0;
static int    Gea2Pdg(int igea);
static int    Pdg2Gea(int ipdg); 
static double Gea2Mas(int igea);  


protected:
int mNTk;


};

#endif

