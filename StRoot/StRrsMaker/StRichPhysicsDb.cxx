/******************************************************
 * $Id: StRichPhysicsDb.cxx,v 1.1 2000/01/18 21:32:03 lasiuk Exp $
 *
 * Description:
 *  Implementation of the two databases modules
 *
 ******************************************************
 * $Log: StRichPhysicsDb.cxx,v $
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 ******************************************************/

//#include "TInterpreter.h"              // ROOT
//#include "TSystem.h"
//#include "St_Table.h"
//#include "St_DataSet.h"
//#include "rich_rich.h"
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 *******************************************************************/

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include <algorithm>
#include <iostream.h>

#ifndef ST_NO_NAMESPACES
using std::max_element;
#endif

//SCL
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

    //RRS
//namespace StRichRawData {
#endif
    StRichPhysicsDb  *StRichPhysicsDb::p2Db  = 0;
//RRS
    StRichPhysicsDb::StRichPhysicsDb()
	: e_distribut(21)
    {    
	//star_fill();
	my_fill();
	common_fill();
    }
    my_fill();
	version         = 1;
    void StRichPhysicsDb::my_fill()
	polia           = 0.3;
	mVersion         = 1;
    
	mPolia           = 0.3;
	avg_n_inter     = 25;
	
	phot2elec       = 0.27;
	phot2pad        = 0.75;
	avl2phot        = 7.7e-6;
	
	electric_noise  = 800.0;
	pedestal        = 50;
	adc_factor      = 0.16556;      // femtocoulomb/adc_channel
	adc_threshold   = 5;
	channel_width   = 10;           // bits
    }
    
    e_distribut[17] = 99.879;

    void StRichPhysicsDb::common_fill()
    {
	e_charge        = 1.602e-19;
	
	e_distribut[0]  = 79.4;
	e_distribut[1]  = 91.4;
	e_distribut[2]  = 94.8;
	e_distribut[3]  = 96.4;
	e_distribut[4]  = 97.35;
	e_distribut[5]  = 97.95;
	e_distribut[6]  = 98.39;
	e_distribut[7]  = 98.73;
	e_distribut[8]  = 99.00;
	e_distribut[9]  = 99.21;
	e_distribut[10] = 99.38;
	e_distribut[11] = 99.51;
	e_distribut[12] = 99.61;
	e_distribut[13] = 99.69;
	e_distribut[14] = 99.75;
	e_distribut[15] = 99.80;
	e_distribut[16] = 99.842;
	e_distribut[17] = 99.879;
	e_distribut[18] = 99.912;
	e_distribut[19] = 99.941;
	e_distribut[20] = 99.941;
	
    e_distribut[18] = 99.912;
	e_max = *(max_element( &e_distribut[0], &e_distribut[20] ));
    e_distribut[20] = 99.941;
    }
    // find max of e_distribut

    /*
     *  Function handles the access to the
     *  only instance by using a static
     *  pointer. 
     */

    StRichPhysicsDb* StRichPhysicsDb::getDb()
    {
	if(!p2Db)
	    p2Db = new StRichPhysicsDb();
	return p2Db;
    }

/*
 *  A fill from a central DB works the following way. 
 *  A STAR_Table that can hold the values is created. It
 *  should be found in the central repository of such tables.
 *  A macro, located in the central repository of DB macros
 *  fills the table with numbers. Then I copy those numbers
 *  to my DBs.
 */

// void StRichPhysicsDb::star_fill()
// {
//     gInterpreter->ProcessLine(".L rich_rich.C");
//     St_DataSet * r = (St_DataSet *) gInterpreter->Calc("CreateTable()");
//     rich_rich_st * s=(rich_rich_st*)((St_Table*)r)->GetArray();
    
//     if ( !s ) cerr << "Error reading from STAR_TABLE!!!\n";
    
//     version         = s->version;
    
//     polia           = s->polia;
//     avg_n_inter     = s->avg_n_inter;
    
//     phot2elec       = s->phot2elec;
//     phot2pad        = s->phot2pad;
//     avl2phot        = s->avl2phot;
    
//     electric_noise  = s->electric_noise;
//     pedestal        = s->pedestal;
//     adc_factor      = s->adc_factor;      // femtocoulomb/adc_channel
//     adc_threshold   = s->adc_threshold;
    os << "adcThreshold= " << adcThreshold() << endl;
    os << "adcChannelWidth= " << adcChannelWidth() << endl;
    os << "adcChannelWidth= "      << adcChannelWidth()               << " channels" << endl;
    os << "electronicNoiseLevel= " << electronicNoiseLevel() << endl;
    //os << "electronCharge= " << electronCharge() << endl;
    os << "\n***************** End of Physics DB ********************************\n" << endl;
}
#ifndef ST_NO_NAMESPACES
//}
#endif
