// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmc_Exception
// \author Piotr A. Zolnierczuk             
// \date   Aug 26, 2002
#ifndef EEmc_Exception_h
#define EEmc_Exception_h


enum EEmcErrno_t {
  kEEmcOK            =  0  ,
  kEEmcUnknownError        ,
  kEEmcMCMissingEventHeader,
  kEEmcMCInvalidDepth      ,
  kEEmcInvalidSector       ,
  kEEmcInvalidSubSector    ,
  kEEmcInvalidEta          ,
  kEEmcMaxErrno            ,
  kEEmcMCErr1     
};


class EEmcException {
public:

  EEmcException();
  EEmcException(const EEmcErrno_t e, const char *msg=NULL, const int value=0);

  EEmcErrno_t getErrno() { return mErrno; };

protected:
  EEmcErrno_t mErrno;
};


#endif
