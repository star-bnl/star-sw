//StKtCluJetFinder.h
//M.L. Miller (Yale Software)
//5/02

#ifndef StKtCluJetFinder_HH
#define StKtCluJetFinder_HH

class StKtCluJetFinder : public StJetFinder
{
public:
    StKtCluJetFinder();
    virtual ~StKtCluJetFinder();

    //action

    /*! Pass a list of protojets.  This list will be packed with jets+beam jets after..
      The user is responsible for filtering the jets.
    */
    virtual void findJets(JetList& protojets);
    virtual void clear() {}; //no-op
    virtual void print() {}; //no-op

private:
};

#endif

