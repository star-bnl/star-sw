
#ifndef StKtCluJetFinder_HH
#define StKtCluJetFinder_HH

/*!
  \class StKtCluPars
  \author M.L. Miller (MIT Software)
  A small class to encapsulate the requisite run-time parameters of the KtClu jet algorithm
 */

/*!
  \class StKtCluJetFinder
  \author M.L. Miller (Yale Software)
  Implementation of the Ellis/Soper kt-cluster algorithm
 */

class StKtCluPars : public StJetPars
{
public:

    ///Set the distance measure, called d by Ellis/Soper
    void setR(double r) {mR=r;}
    double r() const {return mR;}

    ///Toggle the debug stream on/off
    void setDebug(bool v) {mDebug = v;}
    bool debug() const {return mDebug;}

private:
    double mR;
    bool mDebug;
    ClassDef(StKtCluPars,1)
};


class StKtCluJetFinder : public StJetFinder
{
public:
    StKtCluJetFinder(const StKtCluPars& pars);
    virtual ~StKtCluJetFinder();

    /*! Pass a list of protojets.  This list will be packed with jets+beam jets after..
      The user is responsible for filtering the jets. */
    virtual void findJets(JetList& protojets);

    ///No operation
    virtual void clear() {};
    
    ///No operation
    virtual void print() {};

private:
    ///not implemented
    StKtCluJetFinder();
    StKtCluPars mPars;
};

#endif

