/***************************************************************************
 *
 * $Id: StParticleTable.cc,v 1.29 2017/10/30 15:23:54 jwebb Exp $
 *
 * Author: Thomas Ullrich, May 99 (based on Geant4 code, see below) 
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the 
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 ***************************************************************************
 *
 * $Log: StParticleTable.cc,v $
 * Revision 1.29  2017/10/30 15:23:54  jwebb
 * Add K*0(892) aka pdg 313 to gstar with g3id = 10013
 *
 * Revision 1.28  2016/11/28 21:52:43  jwebb
 * Add psi(2s) --> mu+mu- with 100% branching ratio.
 *
 * Revision 1.27  2016/07/29 15:38:51  jwebb
 * Fixed error in G3ID to PDGid mapping.
 *
 * Revision 1.26  2015/07/31 21:32:58  jwebb
 * Attempt to propagate a PDG id for antideuteron.
 *
 * Revision 1.25  2015/06/23 14:53:36  jwebb
 * StarClassLibrary support for H0 dibaryon.
 *
 * Revision 1.24  2015/06/08 16:24:34  jwebb
 * Correct mapping between G3 id and pdg id.  The G3 is the key and must be unique.  THe pdg id is the value, and can repeat.
 *
 * Revision 1.23  2015/06/05 14:01:34  jwebb
 * JPsi (id=168, dimuon channel) added to particle table
 *
 * Revision 1.22  2014/06/25 14:19:24  jwebb
 * Added psi prime --> e+e-
 *
 * Revision 1.21  2014/01/29 16:21:43  jwebb
 * Added D_star_plus (minus) --> D0 (bar) pi+ (-) 100% BR
 *
 * Revision 1.20  2013/04/18 20:05:29  jwebb
 * Ticket #2574 Wrong Omega/Anti-Omega Geant ID.
 *
 * PDG +3334 id was incorrectly assigned to the Omega+... should be -3334 for
 * Omega+ and +3334 for Omega-.  IDs have been swapped.
 *
 * Revision 1.19  2013/03/14 18:27:26  jwebb
 * Added pi0    --> e+e- gamma 100% gid=10007
 * Added K0long --> nu e- pi+  100% gid=10010
 * Added K0long --> nu e+ pi-  100% gid=10110
 *
 * http://www.star.bnl.gov/rt2/Ticket/Display.html?id=2549
 *
 * Revision 1.18  2013/01/31 18:21:46  jwebb
 * Updated StarClassLibrary and gstar_part.g to add the H Dibaryon.
 *
 * Revision 1.17  2012/06/25 16:02:05  jwebb
 * Added Xi0(1530).
 *
 * Revision 1.16  2011/08/29 20:28:26  jwebb
 * Added K+ --> e+ pi0 nu and K- --> e- pi0 nu to satisfy an emedding
 * request http://drupal.star.bnl.gov/STAR/starsimrequests/2010/aug/31/ke3-pp-200-gev-run9
 *
 * Also added the other top 6 decay modes as 10011 -- 15011 and 10012 -- 15012.
 *
 * Revision 1.15  2011/08/12 15:32:54  jwebb
 *
 * Added anti-hypertriton.  Mapped hyper-triton and anti-hyper-triton to
 * geant IDs 6[12]053 and 6[12]054, with two decay modes:
 *
 *     H3(lambda) --> He3 pi-    61053    antiparticle=61054
 *     H3(lambda) --> d p pi-    62053    antiparticle=62054
 *
 * Revision 1.14  2011/07/20 17:57:52  jwebb
 *
 * Updated StParticleTable to provide access to anti-nuclei via the "geant" ID.
 *
 * Revision 1.13  2011/03/30 17:32:50  jwebb
 * Added anti-nuclei (deuteron, triton, alpha and helium3) to the table.
 *
 * Revision 1.12  2011/03/25 18:17:46  jwebb
 * Updates to StParticleTable and additions to STAR Class Library
 *
 * (1) resolve ticket 2097
 * (2) include all particles/deays defined in gstar_part.g
 * (3) added few anti-nuclei in anticipation of future needs.
 * (3) added the geantino for completeness
 *
 * Revision 1.11  2011/03/15 22:41:43  jwebb
 * (1) Added Omega+/- and Xi+/- embedding definitions.
 * (2) 1st attempt at improving the Doxygenization of the particle table.
 *
 * Revision 1.10  2010/08/03 13:51:31  jwebb
 * LambdaBar(1520) provided in particle table with Geant ID=996.
 *
 * Revision 1.9  2010/05/07 15:37:11  jwebb
 * Added StKaonZeroMode0809 to represent the k0 --> pi+ pi- w/ 100% br in
 * gstar_part.g.
 *
 * Revision 1.8  2010/04/06 14:16:22  jwebb
 * Redefined the geant ID of the lambda(1520) from 706 to 995, to make
 * consistent with usage in embedding group.
 *
 * Revision 1.7  2010/03/22 21:32:34  jwebb
 * And set the pdg ID to geant ID mapping.
 *
 * Revision 1.6  2010/01/28 21:54:20  jwebb
 * Added the Sigma(1385) baryons.
 *
 * Revision 1.5  2010/01/28 20:05:20  jwebb
 * Modifications to StParticleTable.cc (1) add the 'new' particle classes
 * defined on 01/28/10 to the table and, (2) add the existing J/Psi and B
 * mesons to the table.  Also defined a preprocessor macro to make reading
 * and modifying the code a bit easier.
 *
 * Revision 1.4  2000/04/06 22:25:38  ullrich
 * Added phi and omega. More STAR specific Geant IDs.
 *
 * Revision 1.3  1999/12/21 15:14:23  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/09/24 01:23:52  fisyak
 * Reduced Include Path
 *
 * Revision 1.1  1999/05/14 18:48:14  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "StarPDGEncoding.hh"
#define kUndefined _undefined_particle_id++
long _undefined_particle_id = 2000000000; /* Unique PDG ID for each undefined particle */

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
#include <ospace/stl/src/treeaux.cpp> // CC4.2 with ObjectSpace only
#endif

#include "StAntiDeuteron.hh"
#include "StAntiTriton.hh"
#include "StAntiAlpha.hh"
#include "StAntiHelium3.hh"
#include "StAntiHyperTriton.hh"
#include "StHyperTriton.hh"
#include "StHDibaryon.hh"


StParticleTable* StParticleTable::mParticleTable = 0;

StParticleTable::~StParticleTable() {/* noop */}

/// Helper function to define PDG ids for heavy ions                                                                                                                                                           
/// @param z Charge of the heavy ion                                                                                                                                                                           
/// @param a Atomic number of the heavy ion                                                                                                                                                                    
/// @param l Number of lambdas in a hypernucleus                                                                                                                                                               
Int_t hid( Int_t z, Int_t a, Int_t l=0 )
{
  //         10LZZZAAAI                                                                                                                                                                                        
  return (   1000000000
         +     10000000*l
         +        10000*z
	     +           10*a );
}



StParticleTable::StParticleTable()
{
    //
    // Setup Geant3 -> PDG table
    // Note, the STAR specific definitions
    //
    typedef mGeantPdgMapType::value_type geantPdgPairType;

    // Helper macro to map geant ID to PDG ID.  A "DCAY" mode is also specificied, and doxygen comments
    // added to the source code.
#define Geant2Pdg(X,Y, DCAY) {				\
      /** DCAY	*/					\
      /** Geant ID: X	*/				\
      /** PDG ID:   Y	*/				\
      /** */						\
      mGeantPdgMap.insert(geantPdgPairType(X,Y));	\
    }


    ///@addtogroup GEANT_STANDARD
    ///@{
    /// Geant3 Standard Particle Defintions
    Geant2Pdg(1, 22, gamma);      
    Geant2Pdg(2, -11, e+);     // e+
    Geant2Pdg(3, 11, e-);      // e-
    Geant2Pdg(4, 12, neutrino);      // neutrino (ambigious)
    Geant2Pdg(5, -13, mu+);     // mu+
    Geant2Pdg(6, 13, mu-);      // mu-
    Geant2Pdg(7, 111, pi0);     // pi0
    Geant2Pdg(8, 211, pi+);     // pi+
    Geant2Pdg(9, -211, pi-);    // pi-
    Geant2Pdg(10, 130, K0_Long);    // K0_long
    Geant2Pdg(11, 321, Kaon+ );    // K+
    Geant2Pdg(12, -321, Kaon- );   // K-
    Geant2Pdg(13, 2112, neutron);   // n
    Geant2Pdg(14, 2212, proton);   // p
    Geant2Pdg(15, -2212, antiproton);  // anti_p
    Geant2Pdg(16, 310, K0_short);    // K0_short
    Geant2Pdg(17, 221, eta);    // eta
    Geant2Pdg(18, 3122, lambda);   // lambda
    Geant2Pdg(19, 3222, sigma+);   // sigma+
    Geant2Pdg(20, 3212, sigma0);   // sigma0
    Geant2Pdg(21, 3112, sigma-);   // sigma-
    Geant2Pdg(22, 3322, Xi0);   // Xi0
    Geant2Pdg(23, 3312, XiMinus);   // Xi-
    Geant2Pdg(24, 3334, Omega);   // Omega
    Geant2Pdg(25, -2112, AntiNeutron );  // anti_n
    Geant2Pdg(26, -3122, AntiLambda );  // anti_lambda
    Geant2Pdg(27, -3222, AntiSigma- );  // anti_sigma-
    Geant2Pdg(28, -3212, AntiSigma0 );  // anti_sigma0
    Geant2Pdg(29, -3112, AntiSigma+ );  // anti_sigma+
    Geant2Pdg(30, -3322, AntiXi0 );  // anti_Xi0
    Geant2Pdg(31, -3312, AntiXi+ );  // anti_Xi+
    Geant2Pdg(32, -3334, AntiOmega+ );  // anti_omega+ 
    Geant2Pdg(33, -15,   AntiTau    );    // anti_tau (STAR def.)
    Geant2Pdg(34, 15,    Tau);     // tau (STAR def.)
    Geant2Pdg(35, 411,   D+ );    // D+  (STAR def.)
    Geant2Pdg(36, -411,  D- );   // D-  (STAR def.)
    Geant2Pdg(37, 421,   D0);    // D0  (STAR def.)
    Geant2Pdg(38, -421,  AntiD0 );   // anti_D0 (STAR def.)
    Geant2Pdg(39, 431,   Ds+ );    // Ds+ (STAR def.)
    Geant2Pdg(40, -431,  Ds- );   // Ds- (STAR def.)
    Geant2Pdg(41, 4122,  Lambda_c+ );   // lambda_c+ (STAR def.)
    Geant2Pdg(42, 24,    W+ );     // W+  (STAR def.)
    Geant2Pdg(43, -24,   W- );    // W-  (STAR def.)
    Geant2Pdg(44, 23,    Z0 );     // Z0  (STAR def.)

    Geant2Pdg(45, hid(1,2)  , Deuteron ); // The deuteron
    Geant2Pdg(46, hid(1,3)  , Triton )  ; // The triton
    Geant2Pdg(47, hid(2,4)  , Alpha )   ; // The alpha
    Geant2Pdg(48, kUndefined, Geantino ); // The mythical geantino 
    Geant2Pdg(49, hid(2,3)  , Helium3  ); // Helium3
    Geant2Pdg(50, 22,         Cerenkov ); // Cerenkov photons

    Geant2Pdg(54, -hid(2,3) , AntiHelium3 ); // AntiHelium3 );

    ///@} 

    
    ///@addtogroup DEPRECATED
    ///@{
    /// Deprecated definitions.  Please use alternate IDs provided below.
       Geant2Pdg(52, kHyperTriton, HyperTriton ); // Star def. HyperTriton (fake pdg id)
    ///@}

    ///@addtogroup STAR_DEFINITIONS
    ///@{
    /// STAR Revised Particle Definitions
       Geant2Pdg( 60, +413, DStar+ ); // D*+
       Geant2Pdg( 61, -413, DStar- ); // D*-
       Geant2Pdg( 62, +423, DStar0 ); // D*0
       Geant2Pdg( 63, -423, DStar0Bar ); // D*0 bar      



       Geant2Pdg(70, +521, B+);    // B+ meson
       Geant2Pdg(71, -521, B-);    // B- meson
       Geant2Pdg(72, +511, B0);    // B0 meson
       Geant2Pdg(73, -511, B0Bar );    // B0-bar meson
    ///@}


    ///@addtogroup NONSTANDARD_DECAY_MODES
    ///@{
    /// STAR Non-Standard Decay Modes 
       Geant2Pdg( 97, -3122, LambdaBar --> pbar + pi+ );
       Geant2Pdg( 98, +3122, Lambda --> p + pi- );       
    ///@}

    Geant2Pdg( 149, kDalitz, Pi0 --> e+ e- gamma ); // pi0 --> e+ e- gamma

    ///@addtogroup STAR_DEFINITIONS
    ///@{
       Geant2Pdg(150, 223, omega);   // omega meson (STAR def.)
       Geant2Pdg(151, 333, phi);   // phi meson (STAR def.)
       Geant2Pdg(152, 113, rho);   // rho meson (STAR def.)
       Geant2Pdg(153, 213, rho+);   // rho+ meson (STAR def.)
       Geant2Pdg(154, -213, rho-);  // rho- meson (STAR def.)
       Geant2Pdg(155, 311, K0);   // K0 (STAR def.)
       Geant2Pdg(156, -311, K0Bar);  // anti_K0 (STAR def.)
    ///@}
      


    ///@addtogroup DIELECTRONS
    ///@{
    /// Quarkonia in dielectron channel

       Geant2Pdg( 160,    443, JPsi );     // JPsi
       Geant2Pdg( 167, 100443, Psi2c );    // Psi' --> e+e-
       Geant2Pdg( 169, 200443, Psi2c );    // Psi' --> mu+mu-
    
       Geant2Pdg( 161,    553, Upsilon1S); // Upsilon(1S)
       Geant2Pdg( 162, 100553, Upsilon2S); // Upsilon(2S)
       Geant2Pdg( 163, 200553, Upsilon3S); // Uspilon(3S)
    ///@}

    ///@addtogroup DIMUONS
    ///@{
    /// Quarkonia in dimuon channel
       Geant2Pdg( 164,    553, Upsilon1S); // Upsilon(1S) -- mu+ mu- channel w/ incorrect partial width
       Geant2Pdg( 165, 100553, Upsilon2S); // Upsilon(2S) -- mu+ mu- channel w/ incorrect partial width
       Geant2Pdg( 166, 200553, Upsilon3S); // Uspilon(3S) -- mu+ mu- channel w/ incorrect partial width

       Geant2Pdg( 168,    443, JPsi); // JPsi -- mu+ mu- channel w/ incorrect partial widths
    ///@}

    ///@addtogroup NONSTANDARD_DECAY_MODES
    ///@{
       Geant2Pdg( 701, +3224, Sigma(1385)+ ); // Sigma 1385 +
       Geant2Pdg( 702, +3114, Sigma(1385)- ); // Sigma 1385 -
       Geant2Pdg( 703, -3114, SigmaBar(1385)+ ); // Sigma 1385 plus bar
       Geant2Pdg( 704, -3224, SigmaBar(1385)- ); // Sigma 1385 minus bar 
       Geant2Pdg( 707, 100311, K0-->pi+pi- ); // K0 --> pi+ pi- 
    ///@}


    Geant2Pdg( +995, +20003122, Lambda(1520) ); // Lambda 1520 
    Geant2Pdg( +996, -20003122, LamdaBar(1520) ); // Lambda 1520 

    ///@addtogroup Embedding
    /// Embedding particle definitions
    ///@{

    Geant2Pdg( 10007, 111, pi0 --> e+ e- gamma );

    Geant2Pdg( 10010, 130, K0 Long --> nu e- pi+ );
    Geant2Pdg( 10110, 130, K0 Long --> nu e+ pi- );

       Geant2Pdg(10017,  221, eta --> e+ e- gamma);
       Geant2Pdg(10018, 3122, lambda --> p + pi- );
       Geant2Pdg(10026,-3122, lambdaBar --> pbar + pi+ );
       Geant2Pdg(10039,  431,  D_s_+ --> phi + pi+ w/ phi --> K+ K- );
       Geant2Pdg(10040, -431,  D_s_- --> phi + pi- w/ phi --> K+ K- );
       Geant2Pdg(10150,  223,  omega --> e+ e- );
       Geant2Pdg(10151,  333,  phi --> K+ K- );
       Geant2Pdg(11151,  333,  phi --> e+ e- );

    Geant2Pdg(10011, 321,  Kaon+ --> mu+ nu );    // K+
    Geant2Pdg(10012, -321, Kaon- --> mu- nu );   // K-

    Geant2Pdg(11011, 321,  Kaon+ --> pi+ pi0 );    // K+
    Geant2Pdg(11012, -321, Kaon- --> pi- pi0 );   // K-

    Geant2Pdg(12011, 321,  Kaon+ --> 2 pi+ pi- );    // K+
    Geant2Pdg(12012, -321, Kaon- --> 2 pi- pi+ );   // K-

    Geant2Pdg(13011, 321,  Kaon+ --> e+ nu pi0 );    // K+
    Geant2Pdg(13012, -321, Kaon- --> e- nu pi0 );   // K-

    Geant2Pdg(14011, 321,  Kaon+ --> mu+ nu pi0 );    // K+
    Geant2Pdg(14012, -321, Kaon- --> mu- nu pi0 );   // K-

    Geant2Pdg(15011, 321,  Kaon+ --> pi+ pi0 pi0 );    // K+
    Geant2Pdg(15012, -321, Kaon- --> pi- pi0 pi0 );   // K-

    Geant2Pdg(10013, 313, Kstar0 --> K+ pi- ); 


       Geant2Pdg( 10060, +413, DStar+ ); // D*+
       Geant2Pdg( 10061, -413, DStar- ); // D*-
       Geant2Pdg( 10062, +423, DStar0 ); // D*0
       Geant2Pdg( 10063, -423, DStar0Bar ); // D*0 bar      

            
       Geant2Pdg( 40001, -3334, Omega+); 
       Geant2Pdg( 40002,  3334, Omega-); 
       Geant2Pdg( 40003, +3312, XiMinus );       
       Geant2Pdg( 40004, -3312, XiPlus  );      
       Geant2Pdg( 40005, +3322, XiZero );       
       Geant2Pdg( 40006, +3322, XiZeroBar );

       Geant2Pdg( 40007, +3324, XiZero 1530 );
       Geant2Pdg( 40008, -3324, XiZero 1530 bar );

    ///@}

    ///@addtogroup ANTINUCLEI
    /// Definitions of anti nuclei
    ///@{
       Geant2Pdg( 50045, -hid(1,2) , anti-deuteron );
       Geant2Pdg( 50046, -hid(1,3) , anti-triton );
       Geant2Pdg( 50047, -hid(2,4) , anti-alpha );
       Geant2Pdg( 50048, -hid(2,3) , anti-He3 );
    ///@}

    ///@addtogroup ANTIHYPERNUCLEI
    ///Definitions of anti-hypernuclei
    ///@{
       Geant2Pdg( 61053, kHyperTriton,         H3(Lambda) --> He3 piminus );
       Geant2Pdg( 61054, kAntiHyperTriton, AntiH3(Lambda) --> AntiHe3 piplus );
       Geant2Pdg( 62053, kHyperTriton,         H3(Lambda) --> d p piminus );
       Geant2Pdg( 62054, kAntiHyperTriton, AntiH3(Lambda) --> dbar pbar piplus );	
    ///@}

    ///@addtogroup EXOTICS
    ///Definitions of exotics, e.g. H-dibaryon
    ///@{
       Geant2Pdg( 60001, kUndefined,         H-Dibaryon --> Lambda + piminus + proton );

       Geant2Pdg( 60801, 801,                H0-strangelet --> proton + Sigma- );

    ///@}


#undef Geant2Pdg

}

StParticleTable::StParticleTable(const StParticleTable &) {/* private */}
   
StParticleTable* StParticleTable::instance()
{
    return particleTable();
}

StParticleTable* StParticleTable::particleTable()
{
    if (!mParticleTable) mParticleTable =  new StParticleTable;
    return mParticleTable;
}

unsigned int StParticleTable::entries() const {return mNameMap.size();}

unsigned int StParticleTable::size() const {return mNameMap.size();}

bool StParticleTable::contains(const string& name) const
{
    return (findParticle(name) != 0);
}

bool StParticleTable::contains(int pdgId) const
{
    return (findParticle(pdgId) != 0);
}
    
bool StParticleTable::containsGeantId(int geantId) const
{
    return (findParticleByGeantId(geantId) != 0);
}
 
StParticleDefinition* StParticleTable::findParticle(const string& name)  const
{
    mNameMapType::const_iterator i = mNameMap.find(name);
    if (i == mNameMap.end())
	return 0;
    else
 	return (*i).second;
}

StParticleDefinition* StParticleTable::findParticle(int pdgId)  const
{
    mPdgMapType::const_iterator p =  mPdgMap.find(pdgId);
    if (p == mPdgMap.end())
	return 0;
    else
 	return (*p).second;
}
    
StParticleDefinition* StParticleTable::findParticleByGeantId(int geantId) const
{
    //
    //  Two ways to find the particle:
    //  1. If it's an elementary particle its in the PDG list 
    //  2. If it is a nucleus/ion find it via the name list
    //

    StParticleDefinition *p = 0;
    switch (geantId) {
    case 45:
	p = findParticle(string("deuteron"));
	break;
    case 46:
	p = findParticle(string("triton"));
	break;
    case 47:
	p = findParticle(string("alpha"));
	break;
    case 49:
	p = findParticle(string("He3"));
	break;
    case 50:
	p = findParticle(string("opticalphoton"));
	break;
    case 50045:
        p = StAntiDeuteron::instance();
	break;
    case 50046:
        p = StAntiTriton::instance();
        break;
    case 50047:
        p = StAntiAlpha::instance();
        break;

    case 50049:
    case 54:
        p = StAntiHelium3::instance();
        break;

    case 60053:
    case 61053:
    case 62053:
      p = StHyperTriton::instance();
      break;
    case 60054:
    case 61054:
    case 62054:
      p = StAntiHyperTriton::instance();      
      break;
      
      
    case 60001:
      p = StHDibaryon::instance();
      break;




    default:
	mGeantPdgMapType::const_iterator i =  mGeantPdgMap.find(geantId);
	if (i != mGeantPdgMap.end())
	    p = findParticle((*i).second);
	break;
    }
    return p;
}

void StParticleTable::insert(StParticleDefinition* p)
{
    typedef mPdgMapType::value_type pdgPairType;
    typedef mNameMapType::value_type namePairType;

    if (p->pdgEncoding() != 0)
	mPdgMap.insert(pdgPairType(p->pdgEncoding(), p));
    mNameMap.insert(namePairType(p->name(), p));
}

void StParticleTable::erase(StParticleDefinition* p)
{
    mPdgMapType::iterator i =  mPdgMap.find(p->pdgEncoding());
    if (i != mPdgMap.end()) mPdgMap.erase(i);
    
    mNameMapType::iterator j =  mNameMap.find(p->name());
    if (j != mNameMap.end()) mNameMap.erase(j);   
}

void StParticleTable::dump(ostream& os)
{
    mNameMapType::iterator i;
    for (i = mNameMap.begin(); i != mNameMap.end(); ++i)
	cout << *((*i).second) << endl;
}


StVecPtrParticleDefinition
StParticleTable::allParticles() const
{
    StVecPtrParticleDefinition vec;
    mNameMapType::const_iterator i;
    for (i = mNameMap.begin(); i != mNameMap.end(); ++i)
	vec.push_back((*i).second);
    return vec;
}




