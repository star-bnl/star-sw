/* Written 11/23/99 cle
 */

/******************* NOTE ********************************************
 * This file should never be included directly. It is included from  * 
 * /DAQ/include/daqFormats.h after the necessary structs are defined *
 *********************************************************************/

#ifndef _HLT_FORMATS_H
#define _HLT_FORMATS_H

//#define HLT_GL3_VERSION	0x20100107
#define HLT_GL3_VERSION	0x20110114  ///< add di-electron selection method bits

// Global tracks:

namespace star {
	namespace rts {
		namespace hlt {
			enum EHLTBANKS {
				    kHLT_NONE
					,kHLT_EVE
					,kHLT_TOF
					,kHLT_PVPD
					,kHLT_EMC
					,kHLT_GT
					,kHLT_PT
					,kHLT_NODE
					,kHLT_HIPT
					,kHLT_DIEP
					,kHLT_HF
			};

			const unsigned int NMaxGTracks = 10000;        ///< number of maximum global tracks
			const unsigned int NMaxPTracks = 10000;        ///< number of maximum primary tracks
			const unsigned int NMaxTofHits = 10000;        ///< number of maximum btof hits
			const unsigned int NMaxPvpdHits = 10000;       ///< number of maximum pvpd hits
			const unsigned int NMaxNodes = 10000;          ///< number of maximum nodes
			const unsigned int NMaxBemcTowers = 4800;      ///< number of bemc towers
			const unsigned int NMaxSN = 1000;              ///< Maximum series number

			struct hlt_track {
				int id ;                  ///< primary key
				unsigned short flag ;     ///<  Primaries flag=1, Secondaries flag=0
				char innerMostRow ;
				char outerMostRow ;
				unsigned char nHits ;     ///<  Number of points assigned to that track
				char reserved ; 
				unsigned char ndedx;      ///<  nr of clusters contributing to the dedx value
				char q ;                  ///<  charge
				float chi2[2];            ///<  chi squared of the momentum fit
				float dedx;               ///<  dE/dx information
				float pt ;                ///<  pt (transverse momentum) at (r,phi,z)
				float phi0;               ///<  azimuthal angle of the first point
				float psi ;               ///<  azimuthal angle of the momentum at (r,..
				float r0 ;                ///<  r (in cyl. coord.) for the first point
				float tanl;               ///<  tg of the dip angle at (r,phi,z)
				float z0 ;                ///<  z coordinate of the first point
				float length ;
				float dpt ;
				float dpsi;
				float dz0 ;
				float dtanl ;
			}; //16 dwords

			///<  Bank which actually has the global tracks in it:
			///<  compared to the sector level tracks this merges the pointer and data bank
			///<  into one Bank.

			struct HLT_GT {
				unsigned int      nGlobalTracks;
				struct hlt_track globalTrack[NMaxGTracks];
			};

			struct HLT_PT {
				unsigned int      nPrimaryTracks;
				struct hlt_track primaryTrack[NMaxPTracks];
			};

			struct hlt_TofHit{
				short trayId;
				short channel;///<  = nModule*6+nCell
				float tdc;
				float tot;
				float tof;
				float triggertime;
			};
			///<  Bank of tof or pvpd hits


			struct HLT_TOF {
				unsigned int      nTofHits;
				struct hlt_TofHit tofHit[NMaxTofHits];
			};

			struct HLT_PVPD {
				unsigned int      nPvpdHits;
				struct hlt_TofHit pvpdHit[NMaxPvpdHits];
			};

			struct hlt_emcTower {
				int   adc;
				float energy;

				float phi; 
				float eta;
				float z;
				int softId;
				int daqId;
			};
			///<  Bank of bemc tower hits

			struct HLT_EMC {
				unsigned int      nEmcTowers; 
				struct hlt_emcTower emcTower[NMaxNodes];
			};

			struct hlt_node {
				int globalTrackSN;   ///<  serial number in HLT_GT
				int primaryTrackSN;  ///<  serial number in HLT_PT
				int tofHitSN;        ///<  serial number in HLT_TOF
				int emcTowerSN;      ///<  serial number in HLT_EMC

				double emcMatchPhiDiff;
				double emcMatchZEdge;

				int projChannel;
				float localY;
				float localZ;
				float beta;
				float tof;
				float pathlength;
			};
			///<  Bank of nodes

			struct HLT_NODE {
				unsigned int      nNodes; 
				struct hlt_node node[NMaxBemcTowers];
			};

			struct HLT_EVE {
				unsigned int version ;
				unsigned int hltDecision;  ///<  HLT trigger bits
				float vertexX;
				float vertexY;
				float vertexZ;
				float lmVertexX;
				float lmVertexY;
				float lmVertexZ;
				float vpdVertexZ;
				float T0;
				float innerSectorGain;
				float outerSectorGain;

			};
			///<  Bank with event level qualities

			struct hlt_diElectronPair {
				int dau1NodeSN;
				int dau2NodeSN;
				float invariantMass;
				float pt;
				float psi;
				float tanl;
				int dau1SelectionBit;
				int dau2SelectionBit;
			};
			///<  Bank of di-electron

			///<  di-electron pair bank
			struct HLT_DIEP {
				unsigned int      nEPairs; 
				struct hlt_diElectronPair ePair[NMaxSN];
			};

			///<  high pt bank
			struct HLT_HIPT {
				unsigned int      nHighPt; 
				int highPtNodeSN[NMaxSN];
			};

			///<  heavy fragment bank
			struct HLT_HF {
				unsigned int      nHeavyFragments; 
				int heavyFragmentSN[NMaxSN];
			};

		}  // hlt
	}  // rts
}  // star
#endif
