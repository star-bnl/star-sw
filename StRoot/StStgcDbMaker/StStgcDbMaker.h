#ifndef STSTGCDBMAKER_H
#define STSTGCDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StThreeVectorD.hh"
#include "StLorentzVectorD.hh"

#include "St_db_Maker/St_db_Maker.h"
#include "StEvent/StEnumerations.h"

class StStgcDbMaker : public StMaker {
	public: 
	StStgcDbMaker(const Char_t *name="stgcDb");
	virtual       ~StStgcDbMaker();

	virtual Int_t  Init() {
		return kStOK;
	}
#if !defined(__CINT__) && !defined(__CLING__)
	virtual Int_t  InitRun(Int_t runNumber) {
		LOG_INFO << "StStgcDbMaker::InitRun - run = " << runNumber << endm;
		//! Accessing DBs
		if(mDebug>0) {
			St_db_Maker* dbmaker = (St_db_Maker*)GetMaker("db");

			if(dbmaker){
				LOG_INFO << "StFcsDbMaker::InitRun - Date&time from St_db_Maker="<<dbmaker->GetDate()<<","<< dbmaker->GetTime() << endm;
			}
		}
		
		makeMap();
		return kStOK;
	}
	virtual Int_t  Make() {LOG_DEBUG<<"StStgcDbMaker Make"<<endm; return kStOK;}
	virtual Int_t  Finish() {LOG_DEBUG<<"StStgcDbMaker Finish"<<endm; return kStOK; }
	virtual void   Clear(const Char_t *opt) {LOG_DEBUG<<"StStgcDbMaker Clear"<<endm; StMaker::Clear();}
#else
	virtual Int_t  InitRun(Int_t runNumber);
	virtual Int_t  Make();
	virtual Int_t  Finish();
	virtual void   Clear(const Char_t *opt);
#endif
	void setRun(Int_t run) { mRun = run;} // set run# 
	void setDebug(Int_t debug=1) { mDebug = debug; } //debug mode, 0 for minimal message, >0 for more debug messages


	UInt_t toId( UShort_t fee, UShort_t altro, UShort_t channel ){
		return ( fee & 0xFF ) + (( altro & 0xFF ) << 8) + (( channel & 0xFF ) << 16);
	}
	void fromId( UInt_t id, UShort_t &fee, UShort_t &altro, UShort_t &channel ){
		fee = id & 0xFF;
		altro = (id >> 8 ) & 0xFF;
		channel = (id >> 16) & 0xFF;
		return;
	}

	Int_t stripIndex( UInt_t id ){
		if ( map_id_to_strip.count( id ) < 1 ){
			return -1;
		}
		return map_id_to_strip[ id ];
	}

	Int_t stripOrientation( UInt_t id ){
		UShort_t fee, altro, channel;
		fromId( id, fee, altro, channel );

		if ( 1 == fee || 2 == fee || 4 == fee )
			return kStgcHorizontalStrips;
		return kStgcVerticalStrips;
	}

	void globalPosition( UInt_t id, float &x, float &y, float &dx, float &dy, float &z, float &dz ){
		float localx = 0, localy = 0;
		localPosition( id, localx, localy, dx, dy );
		y = -72 + localy;
		x = 76 + localx;
		z = 670; // just a guess, maybe David did not measure z
		dz = 1;
	}

	void localPosition( UInt_t id, float &x, float &y, float &dx, float &dy ){

		// local coords:
		// +y
		// |
		// |
		// |
		// |
		// |
		// |_________________ +x
		// (0, 0)
		Int_t stripDir = stripOrientation( id );
		Int_t stripIndex = map_id_to_strip[ id ];
		
		const int STGC_HEIGHT = 30; // cm
		const int STGC_WIDTH  = 30; // cm

		if ( kStgcVerticalStrips == stripDir ){
			dx = 0.32; // 3.2 mm -> cm
			dy = STGC_HEIGHT;
			y = STGC_HEIGHT / 2.0;
			x = STGC_WIDTH - dx * stripIndex;
		} else {
			dx = STGC_WIDTH; 
			dy = 0.32; // 3.2 mm -> cm
			y = STGC_HEIGHT - dy * stripIndex;
			x = STGC_WIDTH / 2.0;
		}
	}


#if !defined(__CINT__) && !defined(__CLING__)
	void makeMap(){
		LOG_INFO << "Making Map of sTGC 2019 prototype" << endm;
		// only 2019 prototype!
		map_id_to_strip[ toId( 8, 16, 8 ) ] = 1;
		map_id_to_strip[ toId( 8, 16, 9 ) ] = 2;
		map_id_to_strip[ toId( 8, 16, 10 ) ] = 3;
		map_id_to_strip[ toId( 8, 16, 11 ) ] = 4;
		map_id_to_strip[ toId( 8, 16, 12 ) ] = 5;
		map_id_to_strip[ toId( 8, 16, 13 ) ] = 6;
		map_id_to_strip[ toId( 8, 16, 14 ) ] = 7;
		map_id_to_strip[ toId( 8, 16, 15 ) ] = 8;
		map_id_to_strip[ toId( 8, 16, 7 ) ] = 9;
		map_id_to_strip[ toId( 8, 16, 6 ) ] = 10;
		map_id_to_strip[ toId( 8, 16, 5 ) ] = 11;
		map_id_to_strip[ toId( 8, 16, 4 ) ] = 12;
		map_id_to_strip[ toId( 8, 16, 3 ) ] = 13;
		map_id_to_strip[ toId( 8, 16, 2 ) ] = 14;
		map_id_to_strip[ toId( 8, 16, 1 ) ] = 15;
		map_id_to_strip[ toId( 8, 16, 0 ) ] = 16;
		map_id_to_strip[ toId( 8, 17, 9 ) ] = 17;
		map_id_to_strip[ toId( 8, 17, 8 ) ] = 18;
		map_id_to_strip[ toId( 8, 17, 11 ) ] = 19;
		map_id_to_strip[ toId( 8, 17, 10 ) ] = 20;
		map_id_to_strip[ toId( 8, 17, 13 ) ] = 21;
		map_id_to_strip[ toId( 8, 17, 12 ) ] = 22;
		map_id_to_strip[ toId( 8, 17, 15 ) ] = 23;
		map_id_to_strip[ toId( 8, 17, 14 ) ] = 24;
		map_id_to_strip[ toId( 8, 17, 6 ) ] = 25;
		map_id_to_strip[ toId( 8, 17, 7 ) ] = 26;
		map_id_to_strip[ toId( 8, 17, 4 ) ] = 27;
		map_id_to_strip[ toId( 8, 17, 5 ) ] = 28;
		map_id_to_strip[ toId( 8, 17, 2 ) ] = 29;
		map_id_to_strip[ toId( 8, 17, 3 ) ] = 30;
		map_id_to_strip[ toId( 8, 17, 0 ) ] = 31;
		map_id_to_strip[ toId( 8, 17, 1 ) ] = 32;
		map_id_to_strip[ toId( 25, 50, 8 ) ] = 33;
		map_id_to_strip[ toId( 25, 50, 9 ) ] = 34;
		map_id_to_strip[ toId( 25, 50, 10 ) ] = 35;
		map_id_to_strip[ toId( 25, 50, 11 ) ] = 36;
		map_id_to_strip[ toId( 25, 50, 12 ) ] = 37;
		map_id_to_strip[ toId( 25, 50, 13 ) ] = 38;
		map_id_to_strip[ toId( 25, 50, 14 ) ] = 39;
		map_id_to_strip[ toId( 25, 50, 15 ) ] = 40;
		map_id_to_strip[ toId( 25, 50, 7 ) ] = 41;
		map_id_to_strip[ toId( 25, 50, 6 ) ] = 42;
		map_id_to_strip[ toId( 25, 50, 5 ) ] = 43;
		map_id_to_strip[ toId( 25, 50, 4 ) ] = 44;
		map_id_to_strip[ toId( 25, 50, 3 ) ] = 45;
		map_id_to_strip[ toId( 25, 50, 2 ) ] = 46;
		map_id_to_strip[ toId( 25, 50, 1 ) ] = 47;
		map_id_to_strip[ toId( 25, 50, 0 ) ] = 48;
		map_id_to_strip[ toId( 25, 51, 9 ) ] = 49;
		map_id_to_strip[ toId( 25, 51, 8 ) ] = 50;
		map_id_to_strip[ toId( 25, 51, 11 ) ] = 51;
		map_id_to_strip[ toId( 25, 51, 10 ) ] = 52;
		map_id_to_strip[ toId( 25, 51, 13 ) ] = 53;
		map_id_to_strip[ toId( 25, 51, 12 ) ] = 54;
		map_id_to_strip[ toId( 25, 51, 15 ) ] = 55;
		map_id_to_strip[ toId( 25, 51, 14 ) ] = 56;
		map_id_to_strip[ toId( 25, 51, 6 ) ] = 57;
		map_id_to_strip[ toId( 25, 51, 7 ) ] = 58;
		map_id_to_strip[ toId( 25, 51, 4 ) ] = 59;
		map_id_to_strip[ toId( 25, 51, 5 ) ] = 60;
		map_id_to_strip[ toId( 25, 51, 2 ) ] = 61;
		map_id_to_strip[ toId( 25, 51, 3 ) ] = 62;
		map_id_to_strip[ toId( 25, 51, 0 ) ] = 63;
		map_id_to_strip[ toId( 25, 51, 1 ) ] = 64;
		map_id_to_strip[ toId( 28, 56, 8 ) ] = 65;
		map_id_to_strip[ toId( 28, 56, 9 ) ] = 66;
		map_id_to_strip[ toId( 28, 56, 10 ) ] = 67;
		map_id_to_strip[ toId( 28, 56, 11 ) ] = 68;
		map_id_to_strip[ toId( 28, 56, 12 ) ] = 69;
		map_id_to_strip[ toId( 28, 56, 13 ) ] = 70;
		map_id_to_strip[ toId( 28, 56, 14 ) ] = 71;
		map_id_to_strip[ toId( 28, 56, 15 ) ] = 72;
		map_id_to_strip[ toId( 28, 56, 7 ) ] = 73;
		map_id_to_strip[ toId( 28, 56, 6 ) ] = 74;
		map_id_to_strip[ toId( 28, 56, 5 ) ] = 75;
		map_id_to_strip[ toId( 28, 56, 4 ) ] = 76;
		map_id_to_strip[ toId( 28, 56, 3 ) ] = 77;
		map_id_to_strip[ toId( 28, 56, 2 ) ] = 78;
		map_id_to_strip[ toId( 28, 56, 1 ) ] = 79;
		map_id_to_strip[ toId( 28, 56, 0 ) ] = 80;
		map_id_to_strip[ toId( 28, 57, 9 ) ] = 81;
		map_id_to_strip[ toId( 28, 57, 8 ) ] = 82;
		map_id_to_strip[ toId( 28, 57, 11 ) ] = 83;
		map_id_to_strip[ toId( 28, 57, 10 ) ] = 84;
		map_id_to_strip[ toId( 28, 57, 13 ) ] = 85;
		map_id_to_strip[ toId( 28, 57, 12 ) ] = 86;
		map_id_to_strip[ toId( 28, 57, 15 ) ] = 87;
		map_id_to_strip[ toId( 28, 57, 14 ) ] = 88;
		map_id_to_strip[ toId( 28, 57, 6 ) ] = 89;
		map_id_to_strip[ toId( 28, 57, 7 ) ] = 90;
		map_id_to_strip[ toId( 28, 57, 4 ) ] = 91;
		map_id_to_strip[ toId( 28, 57, 5 ) ] = 92;
		map_id_to_strip[ toId( 28, 57, 2 ) ] = 93;
		map_id_to_strip[ toId( 28, 57, 3 ) ] = 94;
		map_id_to_strip[ toId( 28, 57, 0 ) ] = 95;
		map_id_to_strip[ toId( 28, 57, 1 ) ] = 96;


		map_id_to_strip[ toId( 1, 2, 8 ) ] = 1;
		map_id_to_strip[ toId( 1, 2, 9 ) ] = 2;
		map_id_to_strip[ toId( 1, 2, 10 ) ] = 3;
		map_id_to_strip[ toId( 1, 2, 11 ) ] = 4;
		map_id_to_strip[ toId( 1, 2, 12 ) ] = 5;
		map_id_to_strip[ toId( 1, 2, 13 ) ] = 6;
		map_id_to_strip[ toId( 1, 2, 14 ) ] = 7;
		map_id_to_strip[ toId( 1, 2, 15 ) ] = 8;
		map_id_to_strip[ toId( 1, 2, 7 ) ] = 9;
		map_id_to_strip[ toId( 1, 2, 6 ) ] = 10;
		map_id_to_strip[ toId( 1, 2, 5 ) ] = 11;
		map_id_to_strip[ toId( 1, 2, 4 ) ] = 12;
		map_id_to_strip[ toId( 1, 2, 3 ) ] = 13;
		map_id_to_strip[ toId( 1, 2, 2 ) ] = 14;
		map_id_to_strip[ toId( 1, 2, 1 ) ] = 15;
		map_id_to_strip[ toId( 1, 2, 0 ) ] = 16;
		map_id_to_strip[ toId( 1, 3, 9 ) ] = 17;
		map_id_to_strip[ toId( 1, 3, 8 ) ] = 18;
		map_id_to_strip[ toId( 1, 3, 11 ) ] = 19;
		map_id_to_strip[ toId( 1, 3, 10 ) ] = 20;
		map_id_to_strip[ toId( 1, 3, 13 ) ] = 21;
		map_id_to_strip[ toId( 1, 3, 12 ) ] = 22;
		map_id_to_strip[ toId( 1, 3, 15 ) ] = 23;
		map_id_to_strip[ toId( 1, 3, 14 ) ] = 24;
		map_id_to_strip[ toId( 1, 3, 6 ) ] = 25;
		map_id_to_strip[ toId( 1, 3, 7 ) ] = 26;
		map_id_to_strip[ toId( 1, 3, 4 ) ] = 27;
		map_id_to_strip[ toId( 1, 3, 5 ) ] = 28;
		map_id_to_strip[ toId( 1, 3, 2 ) ] = 29;
		map_id_to_strip[ toId( 1, 3, 3 ) ] = 30;
		map_id_to_strip[ toId( 1, 3, 0 ) ] = 31;
		map_id_to_strip[ toId( 1, 3, 1 ) ] = 32;
		map_id_to_strip[ toId( 2, 4, 8 ) ] = 33;
		map_id_to_strip[ toId( 2, 4, 9 ) ] = 34;
		map_id_to_strip[ toId( 2, 4, 10 ) ] = 35;
		map_id_to_strip[ toId( 2, 4, 11 ) ] = 36;
		map_id_to_strip[ toId( 2, 4, 12 ) ] = 37;
		map_id_to_strip[ toId( 2, 4, 13 ) ] = 38;
		map_id_to_strip[ toId( 2, 4, 14 ) ] = 39;
		map_id_to_strip[ toId( 2, 4, 15 ) ] = 40;
		map_id_to_strip[ toId( 2, 4, 7 ) ] = 41;
		map_id_to_strip[ toId( 2, 4, 6 ) ] = 42;
		map_id_to_strip[ toId( 2, 4, 5 ) ] = 43;
		map_id_to_strip[ toId( 2, 4, 4 ) ] = 44;
		map_id_to_strip[ toId( 2, 4, 3 ) ] = 45;
		map_id_to_strip[ toId( 2, 4, 2 ) ] = 46;
		map_id_to_strip[ toId( 2, 4, 1 ) ] = 47;
		map_id_to_strip[ toId( 2, 4, 0 ) ] = 48;
		map_id_to_strip[ toId( 2, 5, 9 ) ] = 49;
		map_id_to_strip[ toId( 2, 5, 8 ) ] = 50;
		map_id_to_strip[ toId( 2, 5, 11 ) ] = 51;
		map_id_to_strip[ toId( 2, 5, 10 ) ] = 52;
		map_id_to_strip[ toId( 2, 5, 13 ) ] = 53;
		map_id_to_strip[ toId( 2, 5, 12 ) ] = 54;
		map_id_to_strip[ toId( 2, 5, 15 ) ] = 55;
		map_id_to_strip[ toId( 2, 5, 14 ) ] = 56;
		map_id_to_strip[ toId( 2, 5, 6 ) ] = 57;
		map_id_to_strip[ toId( 2, 5, 7 ) ] = 58;
		map_id_to_strip[ toId( 2, 5, 4 ) ] = 59;
		map_id_to_strip[ toId( 2, 5, 5 ) ] = 60;
		map_id_to_strip[ toId( 2, 5, 2 ) ] = 61;
		map_id_to_strip[ toId( 2, 5, 3 ) ] = 62;
		map_id_to_strip[ toId( 2, 5, 0 ) ] = 63;
		map_id_to_strip[ toId( 2, 5, 1 ) ] = 64;
		map_id_to_strip[ toId( 4, 8, 8 ) ] = 65;
		map_id_to_strip[ toId( 4, 8, 9 ) ] = 66;
		map_id_to_strip[ toId( 4, 8, 10 ) ] = 67;
		map_id_to_strip[ toId( 4, 8, 11 ) ] = 68;
		map_id_to_strip[ toId( 4, 8, 12 ) ] = 69;
		map_id_to_strip[ toId( 4, 8, 13 ) ] = 70;
		map_id_to_strip[ toId( 4, 8, 14 ) ] = 71;
		map_id_to_strip[ toId( 4, 8, 15 ) ] = 72;
		map_id_to_strip[ toId( 4, 8, 7 ) ] = 73;
		map_id_to_strip[ toId( 4, 8, 6 ) ] = 74;
		map_id_to_strip[ toId( 4, 8, 5 ) ] = 75;
		map_id_to_strip[ toId( 4, 8, 4 ) ] = 76;
		map_id_to_strip[ toId( 4, 8, 3 ) ] = 77;
		map_id_to_strip[ toId( 4, 8, 2 ) ] = 78;
		map_id_to_strip[ toId( 4, 8, 1 ) ] = 79;
		map_id_to_strip[ toId( 4, 8, 0 ) ] = 80;
		map_id_to_strip[ toId( 4, 9, 9 ) ] = 81;
		map_id_to_strip[ toId( 4, 9, 8 ) ] = 82;
		map_id_to_strip[ toId( 4, 9, 11 ) ] = 83;
		map_id_to_strip[ toId( 4, 9, 10 ) ] = 84;
		map_id_to_strip[ toId( 4, 9, 13 ) ] = 85;
		map_id_to_strip[ toId( 4, 9, 12 ) ] = 86;
		map_id_to_strip[ toId( 4, 9, 15 ) ] = 87;
		map_id_to_strip[ toId( 4, 9, 14 ) ] = 88;
		map_id_to_strip[ toId( 4, 9, 6 ) ] = 89;
		map_id_to_strip[ toId( 4, 9, 7 ) ] = 90;
		map_id_to_strip[ toId( 4, 9, 4 ) ] = 91;
		map_id_to_strip[ toId( 4, 9, 5 ) ] = 92;
		map_id_to_strip[ toId( 4, 9, 2 ) ] = 93;
		map_id_to_strip[ toId( 4, 9, 3 ) ] = 94;
		map_id_to_strip[ toId( 4, 9, 0 ) ] = 95;
		map_id_to_strip[ toId( 4, 9, 1 ) ] = 96;


	}

#else
	void makeMap();
#endif
	private:
	Int_t   mRun=0;                          //! run#
	Int_t   mDebug=0;                        //! >0 dump tables to text files    
	Int_t   mRun19=0;                        //!
	
	map<UInt_t, Int_t> map_id_to_strip;

	virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
	ClassDef(StStgcDbMaker,1)   //StAF chain virtual base class for Makers    
};

#endif
