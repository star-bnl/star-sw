#include <stdio.h>
#include <iostream.h>
#include <strings.h>
#include <sys/types.h>
#include <stdlib.h>
#include <Rtypes.h> /* use ROOT variables: ..._t */
#include "St_l3banks_2_tphits.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_hitarray_Table.h"
#include "St_l3_Coordinate_Transformer.h"
#include "TStopwatch.h"

ClassImp(St_l3banks_2_tphits);

//______________________________
St_l3banks_2_tphits::St_l3banks_2_tphits(St_tcl_tphit* HITS, St_hitarray* BANK)
{
    mytclhits = HITS;
    mybank = BANK;
    //cout << "erzeugt " << endl;
}
//______________________________
St_l3banks_2_tphits::~St_l3banks_2_tphits()
{
    //cout <<" zerstoert ! " << endl;
}
//______________________________
Int_t St_l3banks_2_tphits::Filltclpoints()
{
    // Prepare transformation
    St_l3_Coordinate_Transformer transformer;

    // Prepare tcl_tphit_stucts to be filled
    tcl_tphit_st* mytclhits_st = (tcl_tphit_st*) mytclhits->GetTable();
    Int_t tphit_index = mytclhits->GetNRows();
    //cout << "rows " << (Int_t) tphit_index  << endl;
   

    // Get Bank
    hitarray_st*  mybank_st = (hitarray_st*) mybank->GetTable();
    UInt_t* bank =  (UInt_t*) mybank_st ;
    Int_t supersector = (Int_t) ((bank[3]+1)/2);
    cout << "Supersector " <<  supersector  << endl;
    // Check it
    if ( 1 > supersector  || supersector > 23 )  
	{
	    cout << "Unuseable banks abort filling of tphit_st structs. " << endl;
	    return -1;
	}

    ////////
    // Loop over receiverboards : 6 in each sector = 12 per supersector
    ////////
    for(Int_t rbindex = 1 ; rbindex <=12 ; rbindex++)
	{
	    // get rb offset
	    Int_t rboffset = (Int_t) bank[10+2*(rbindex-1)];

	    // get sector number 
	    Int_t sector=0;
	    if (rbindex<=6) 
		{ 
		    sector=2*supersector-1; 
		}
	    else if ( rbindex>6 && rbindex <=12 ) 
		{ 
		    sector=2*supersector;
		};

	    //////////
	    // Loop over mezzanine 
	    //////////
	    for(Int_t mzindex = 1 ; mzindex <= 3 ; mzindex++)
		{
	 
		    Int_t mzoffset = (Int_t) bank[rboffset+10+2*(mzindex-1)];
		    Int_t mzlength = (Int_t) bank[rboffset+11+2*(mzindex-1)];

		    // is mz empty ? 
		    if ( mzlength == 11 ) 
			{
			    //    printf("\t\t 0 clusters on this mz \n"); 
			} 
		    else // mz is not empty ! 
			{ 
			    ///////////
			    // Loop over rows
			    ///////////
			    // Get rows
			    Int_t nrows = (Int_t) bank[rboffset+mzoffset+10];
			    // Check it
			    if ( 1 > nrows  || nrows > 6 )  
				{
				    cout << "Unuseable banks abort filling of tphit_st structs. " << endl;
				    return -1;
				}

			    // set row offset for first row
			    Int_t rowoffset = 0;

			    for ( Int_t rowindex = 1 ; rowindex <= nrows ; rowindex++ )
				{ 
				    // get row number  
				    Int_t row = bank[rboffset+mzoffset+10+1+rowoffset];
				    if ( 1 > row  || row > 45 )  
					{
					    cout << "Unuseable banks abort filling of tphit_st structs. " << endl;
					    return -1;
					}

				    // get cluster number  
				    Int_t clusternumb = bank[rboffset+mzoffset+10+2+rowoffset];
		  
				    //////////
				    // Loop over clusters 
				    //////////
				    for ( Int_t clusindex = 1 ; clusindex <= clusternumb ; clusindex++)
					{
					    // define bank dataword		     
					    struct dataword 
					    {
						unsigned short info1;
						unsigned short info2;
					    } *dword;

					    // extract the pad & time center of gravity 
					    UInt_t padinfo = (UInt_t) bank[rboffset+mzoffset+10+2+rowoffset+(2*clusindex-1)];
					    dword = (struct dataword*) &padinfo;
					    Double_t pad  = (Double_t) (dword->info1)/64;
					    Double_t time = (Double_t) (dword->info2)/64;

					    // etract flag & time 
					    UInt_t flaginfo = (UInt_t) bank[rboffset+mzoffset+10+2+rowoffset+(2*clusindex-1)+1];
					    dword= (struct dataword*) &flaginfo;
					    UShort_t flag   = (UShort_t) (dword->info1);
					    UShort_t charge = (UShort_t) (dword->info2);

					    // pad time row sector
					    Double_t ptrs[4];
					    ptrs[0] = pad;
					    ptrs[1] = time;
					    ptrs[2] = row; 
					    ptrs[3] = sector;

					    // transform
					    Double_t* xyz = transformer.raw_to_global(ptrs);
					    
					    // fill tphits
					    mytclhits_st[tphit_index].x = (Float_t) xyz[0];
					    mytclhits_st[tphit_index].y = (Float_t) xyz[1];
					    mytclhits_st[tphit_index].z = (Float_t) xyz[2];
					    mytclhits_st[tphit_index].q = (Float_t) charge;
					    mytclhits_st[tphit_index].row = (Short_t) 100 * sector + row; // to store the sector 
					    mytclhits_st[tphit_index].id  = (Long_t)  1 + tphit_index; // to start with 1

					    // add
					    mytclhits->AddAt(&mytclhits_st[tphit_index],tphit_index);

					    // hit counter
					    tphit_index++;

					   
					} // loop over cluster
				    // set offset for next row 
				    rowoffset += ((2 * clusternumb) + 2); 
				} // loop over rows
			} // mz is not empty
		} // loop over mz's
	} // loop over rb's
   
  
    // Well done go home
    return 1;
   
}
