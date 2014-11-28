/*!
 * \class StFgtPedPlotter 
 * \author S. Gliske, Sept 2011
 */

/***************************************************************************
 *
 * $Id: StFgtPedPlotter.h,v 1.5 2011/09/30 19:09:07 sgliske Exp $
 * Author: S. Gliske, Sept 2011
 *
 ***************************************************************************
 *
 * Description: Makes a plot of the pedistals, reading from either a
 * file.  Make a child class which overwrites the fillData function to
 * make plots from peds in the DB.  Note: is not a maker, nor does it
 * need StChain.
 *
 ***************************************************************************
 *
 * $Log: StFgtPedPlotter.h,v $
 * Revision 1.5  2011/09/30 19:09:07  sgliske
 * general update
 *
 * Revision 1.4  2011/09/29 18:39:43  sgliske
 * Update for geoId->elecCoord function now in StFgtCosmicTestStandGeom
 *
 * Revision 1.3  2011/09/27 00:49:00  sgliske
 * cosmic QA update
 *
 * Revision 1.2  2011/09/24 02:14:10  sgliske
 * updated FGT cosmic QA
 *
 * Revision 1.1  2011/09/22 21:22:01  sgliske
 * creation
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_PED_PLOTTER_
#define _ST_FGT_PED_PLOTTER_

#include <string>
#include <vector>
#include <map>
#include <Rtypes.h>
#include <TGraph.h>

class StFgtPedPlotter {
 public:
   // constructors
   StFgtPedPlotter();

   // default OK
   // StFgtPedPlotter(const StFgtPedPlotter&);

   // equals operator -- default OK
   // StFgtPedPlotter& operator=(const StFgtPedPlotter&);

   // deconstructor
   virtual ~StFgtPedPlotter();

   // make all plots
   Int_t makePlots();

   // accessor
   const TGraph* getGraph( Int_t timebin ) const;
   Float_t getMaxX() const;
   Float_t getMaxY() const;

   // modifiers
   void setReadFromFile( const Char_t* filename );
   void setTimeBinMask( Short_t mask = 0xFF );
   void setPlotVsStrip( Char_t type = 'R' );
   void setPlotStDev( Bool_t doId );
   void setDisc( Short_t discId );
   void setQuad( Short_t quadId );
   void setQuadName( const Char_t *name );

   // typedef
   typedef std::vector< std::vector< Float_t > > VecVec_t;

 protected:
   // since this isn't saved anywhere else
   static const Int_t mMaxNumTimeBins;

   // input filename
   std::string mFileNameIn;

   // mask for which time bins to save
   Short_t mTimeBinMask;

   // what to plot
   Short_t mDiscId, mQuadId;
   Char_t mPlotVsStrip;
   Bool_t mDoPlotStDev;
   std::string mQuadName;

   // to load the data
   virtual Int_t fillData( VecVec_t& X, VecVec_t& Y, VecVec_t& E );

   // for each time bin
   TGraph* makePlot( std::vector< Float_t >& x, std::vector< Float_t >& y, std::vector< Float_t >& e, Int_t timebin );

   // storing the graphs (ped +/- st. dev.)
   std::vector< TGraph* > mGraphVec;

   // for keeping track of time bins
   std::map< Int_t, Int_t > mTimeBinMap;

   // for ease in plotting
   Float_t mMaxY, mMaxX;

 private:
   ClassDef(StFgtPedPlotter,1);

}; 

// inline functions

// constructor
inline StFgtPedPlotter::StFgtPedPlotter() :
   mTimeBinMask( 0x8 ), mMaxY(0), mMaxX(0) { /* */ };

// deconstructor
inline StFgtPedPlotter::~StFgtPedPlotter(){ /* */ };

inline void StFgtPedPlotter::setTimeBinMask( Short_t mask ){ mTimeBinMask = mask; };
inline void StFgtPedPlotter::setDisc( Short_t discId ){ mDiscId = discId; };
inline void StFgtPedPlotter::setQuad( Short_t quadId ){ mQuadId = quadId; };
inline void StFgtPedPlotter::setQuadName( const Char_t* name ){ mQuadName = name; };
inline void StFgtPedPlotter::setReadFromFile( const Char_t* filename ){ mFileNameIn = filename; };

inline void StFgtPedPlotter::setPlotVsStrip( Char_t strip ){
   mPlotVsStrip = ( (strip == 'R' || strip == 'r' || strip == 'P') ? strip : 'c' );
};

inline void StFgtPedPlotter::setPlotStDev( Bool_t doIt ){ mDoPlotStDev = doIt; };

inline Float_t StFgtPedPlotter::getMaxX() const { return mMaxX; };
inline Float_t StFgtPedPlotter::getMaxY() const { return mMaxY; };

#endif
