/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France 
 ****************************************************************************
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 *  class for a Named Object - 
 *  each named Hbt Object shoud inherit from that class
 *
 ***************************************************************************/


#ifndef StHbtNamed_hh
#define StHbtNamed_hh 

class StHbtNamed {

public:
  StHbtNamed() ;
  StHbtNamed(const char * aName) ;
  StHbtNamed (const StHbtNamed&);
  virtual ~StHbtNamed() {delete [] mName;};
  virtual void SetName (const char *aName);
  virtual const char* GetName() const;
       
private:
  char* mName;

#ifdef __ROOT2__
  ClassDef(StHbtNamed,0)
#endif

};

inline const char* StHbtNamed::GetName() const {return mName;};

#endif
