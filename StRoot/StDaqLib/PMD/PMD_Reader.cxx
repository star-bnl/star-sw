/***************************************************************************
 * Author: Susanta and Subhasis 
 *
 * Description: Description : Reads PMD Raw and Pedestals Data and fills PMD Specific Bank.
 *              PMD has been taken as Extended Detector as defined by Tonko.
 *              Designed based on Event Pool Reader of PMD ( Ref: Tonko )
 **************************************************************************/

#include "PMD_Reader.hh"
#include <assert.h>
#define MAX_ADC 0xFFF 

using namespace OLDEVP;

int PMD_Reader::ProcessEvent(const Bank_PMDP * PmdPTR)
{
     int sec, type, ret;
      unsigned int Token;
      Token=PmdPTR->header.Token;
      printf("PMD Token = %d\n", Token);
       
      mThePmd.mode = 0;
      mThePmd.channels = 0;
      mThePmd.max_channels = 2*PMD_CRAMS_MAX*2*PMD_CRAMS_CH_MAX ;  
      

   for(sec=0; sec<2; sec++) {
      if(PmdPTR->sec[sec].length == 0) continue;	   
      Bank_PMDSECP *secp = (Bank_PMDSECP *)(((INT32 *)PmdPTR) + PmdPTR->sec[sec].offset);
  //    cout << "in Process Event==>"<<secp->header.BankType << endl;
      if(strncmp(secp->header.BankType,"PMDSECP",7))
      {
        char str0[40];
        cout<<" PMDSECP error in header name**"<<endl;
        sprintf(str0,"Not PMDSECP");
        ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return 0;
      }

      if(!secp->test_CRC())
      {
        char str0[40];
        cout<<"error in CRC**"<<endl;
        sprintf(str0,"Not PMDSECP");
        ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return 0;
       }
      if(secp->swap() < 0)
      {
       char str0[40];
       cout<<"error in swap**"<<endl;
       sprintf(str0," PMDSECP");
       ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return 0;
      }
     secp->swap();  // Swapping is required to get the offset
     for( type=0; type < 4; type ++) {
	   //  cout<<"sec="<< sec <<"type" <<type << "length="<<secp->type[type].length << endl;
	     if(secp->type[type].length ==0 ) continue;
	     switch(type){
		     case PMD_ADCD_N :
                         {
		       	   // cout<<"adcd off=" << secp->type[type].offset << endl;
                            Bank_PMDADCD *adcd = (Bank_PMDADCD *)(((INT32 *)secp) + secp->type[type].offset);
                            //cout << "in Process Event==>"<<adcd->header.BankType << endl;
                            if(strncmp(adcd->header.BankType,"PMDADCD",7))
                           {
                              char str0[40];
                              cout<<" PMDADCD error in header name**"<<endl;
                              sprintf(str0,"Not PMDADCD");
                              ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return 0;
                            }
			    ret = adcReader(sec, adcd);
                            if(ret) mThePmd.channels += ret;
                           //  printf("after adcReader ret=%d\n", ret);
                         }
			 break; 
			case PMD_PEDR_N :
				{
			      // cout<<"sec"<<sec<<"type="<< type<<"pedr off=" << secp->type[type].offset << endl;
				  Bank_PMDPEDR *pedr = (Bank_PMDPEDR *)(((INT32 *)secp) + secp->type[type].offset);
                                //  cout << "in Process Event==>"<<pedr->header.BankType << endl;
                                  if(strncmp(pedr->header.BankType,"PMDPEDR",7))
                                  {
                                    char str0[40];
                                    cout<<" PMDPEDR error in header name**"<<endl;
                                    sprintf(str0,"Not PMDPEDR");
                                    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return 0;
                                  }
                                  ret = pedReader(sec, type, pedr);
                                  if(ret) mThePmd.channels += ret;
				  mThePmd.mode = 1;
				}
                                  break;

			case PMD_RMSR_N :
				{
			         //cout<<"sec"<<sec<<"type="<< type<<"rmsr off=" << secp->type[type].offset << endl;
				  Bank_PMDPEDR *rmsr = (Bank_PMDPEDR *)(((INT32 *)secp) + secp->type[type].offset);
                           //       cout << "in Process Event==>"<<pedr->header.BankType << endl;
                                  if(strncmp(rmsr->header.BankType,"PMDRMSR",7))
                                  {
                                    char str0[40];
                                    cout<<" PMDRMSR error in header name**"<<endl;
                                    sprintf(str0,"Not PMDRMSR");
                                    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return 0;
                                  }
                                  ret = pedReader(sec, type, rmsr);
                                  if(ret) mThePmd.channels += ret;
				  mThePmd.mode = 1;
				}

                                  break;
			case PMD_THRR_N :
				{
				  Bank_PMDPEDR *thrr = (Bank_PMDPEDR *)(((INT32 *)secp) + secp->type[type].offset);
                           //       cout << "in Process Event==>"<<pedr->header.BankType << endl;
                                  if(strncmp(thrr->header.BankType,"PMDTHRR",7))
                                  {
                                    char str0[40];
                                    cout<<" PMDTHRR error in header name**"<<endl;
                                    sprintf(str0,"Not PMDTHRR");
                                    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return 0;
                                  }
                                  ret = pedReader(sec, type, thrr);
                                  if(ret) mThePmd.channels += ret;
				  mThePmd.mode = 1;
				}
                                  break;
		}
           }
      }
      return 1;
}


int PMD_Reader::adcReader(int sec, Bank_PMDADCD *adcd)
{
 adcd->swap();
 int items = adcd->header.BankLength -(INT32)sizeof(Bank_Header)/4 ;

 unsigned int *datum ;
 unsigned int j ;
 unsigned int cram, blk ;
 unsigned int ch_num ;
 unsigned int ch ;

 if(items <= 0) return 0 ;
		  
		  
 datum = (unsigned int *)adcd->data ;
			  
 ch_num = 0 ;
						  
//printf("PMD sec %d: items %d\n",sec,items) ;
// 
 unsigned int *end_datum = datum + items ;
       
 for(end_datum=datum+items;datum<end_datum;) {
     unsigned int tmp = *datum ;
     datum++ ;

     cram = (tmp & 0xFF000000) >> 24 ;
     blk = (tmp & 0x00FF0000) >> 16 ;
     ch = (tmp & 0x0000FFFF) ;
			   
     mThePmd.no_of_channels_in_cram_blk[sec][cram][blk] = ch;

    if(cram >= PMD_CRAMS_MAX) {
        printf("PMD: Bad Cram number %d\n",cram) ;
        return 0 ;
    }
                                                                                 
    if((blk != 0) && (blk != 1)) {
       printf("PMD: Bad Block %d in Cram %d\n",blk,cram) ;
       return 0 ;
     }
                                                                                                                                                         
    if(ch > PMD_CRAMS_CH_MAX) {
          printf("PMD: too many channels in Cram %d, Block %d: %d\n",cram,blk,ch) ;
          return 0 ;
    }

    for(j=0;j<ch;j++) {
          unsigned int val ;

          val = (*datum) ;
          datum++ ;
							 
          if(val & 0x40000000) {
            unsigned int channel = (val & 0x7ff000) >> 12 ;
	    val &= 0xFFF ;
//	    printf("        %4d: %4d == %4d\n",j,channel,val) ;
	        if(channel >= PMD_CRAMS_CH_MAX) {
	           printf("PMD: channel too big in Cram %d, Block %d: %d\n",cram,blk,channel) ;
	           return 0 ;
	        }
	     mThePmd.adc[sec][cram][blk][channel] = val ;
	     ch_num++ ;
	 }
       }	
 }
return ch_num ;
 
}



PMD_Reader::PMD_Reader(EventReader *er, Bank_PMDP *pPMDP)
{
  pBankPMDP = pPMDP; //copy into class data member for use by other methods
  ercpy = er; // squirrel away pointer eventreader for our friends
  printf(" This PMD_Reader in ctor , %s. \n", __FILE__ );
  
  pBankPMDP->header.BankType[7]=0;
   cout<<"header bank type "<<pBankPMDP->header.BankType<<endl;
  if (!pBankPMDP->test_CRC())  {
    printf("CRC error in PMDP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  if (pBankPMDP->swap() < 0) {
    printf("swap error in PMDP: %s %d\n",__FILE__,__LINE__) ;
  }
  
  pBankPMDP->header.CRC = 0;
  
  //do whatever else needs to be done
    int Token = pBankPMDP->header.Token;
  Bank_DATAP *dp = (Bank_DATAP *)ercpy->getDATAP(); 
  if(Token !=dp->header.Token){
    printf("Token mismatch between global %d and PMD %d\n",dp->header.Token,Token);
  }

 for(int sec=0; sec < 2; sec++){
	for(int cram=0; cram<PMD_CRAMS_MAX; cram++){
		for(int blk=0; blk < 2; blk ++){
			for(int channel=0; channel<PMD_CRAMS_CH_MAX; channel++){
				mThePmd.adc[sec][cram][blk][channel] = 0;
				mThePmd.ped[sec][cram][blk][channel] = 0;
				mThePmd.rms[sec][cram][blk][channel] = 0;
				mThePmd.thr[sec][cram][blk][channel] = 0;
			}
		}
	}
 }
//  mTheRichArray.ByteSwapped = 0x04030201;  // This is to signal Endianness if struct is saved
  
  
  ProcessEvent(pBankPMDP);    // switch locations in little endian machines
  
 }
Bank_DATA PMD_Reader::getPMD_ADC()
{
	return mThePmd;
}

int PMD_Reader::NPMDHits()
{
 /* if(mPmdPresent)
    {
      if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8))
      {
        cout<<"EMC_Reader::NTowerHits() -> error in header"<<endl;
        return 0;
      }
    
    int nhits=mThePmdAdcD.NPPMDHits;
*/
    int nhits = 10;
    return nhits;
//    }
    //else return 0;
}

int PMD_Reader::NCPVHits()
{
 /* if(mPmdPresent)
    {
      if(strncmp(mTheTowerAdcR.BankType,"TOWRADCR",8))
      {
        cout<<"EMC_Reader::NTowerHits() -> error in header"<<endl;
        return 0;
      }
    
    int nhits=mThePmdAdcD.NPPMDHits;
*/
    int nhits = 10;
    return nhits;
//    }
    //else return 0;
}

int PMD_Reader::pedReader(int sec, int type, Bank_PMDPEDR *pedr)
{
      pedr->swap();
      int items = pedr->header.BankLength -(INT32)sizeof(Bank_Header)/4 ;
      printf("intem = %d, 1st=%d, 3nd=%d\n", items, pedr->header.BankLength, (INT32)sizeof(Bank_Header)/4);
	unsigned int *datum ;
	int cram, blk, ch ;
	int ch_num ;
	int valid ;

	if(items <= 0) return 0 ;

        datum = (unsigned int *)pedr->data ;

	ch_num = 0 ;

	//printf("PMD sec %d: items %d\n",sec,items) ;

        unsigned int *end_datum = datum + items ;

	for(end_datum=datum+items;datum<end_datum;) {
		int channel ;
		unsigned int tmp = (*datum) ;
		datum++ ;

		cram = (tmp & 0x7F00) >> 8 ;
		blk = (tmp & 0x00FF)  ;

		valid = tmp & 0x8000 ;


		ch = PMD_CRAMS_CH_MAX ;

		if(!valid) {
			//printf("Datum not valid at %d == 0x%04X",end_datum, tmp) ;

			datum += ch ;
			continue ;
		}


		if(cram >= PMD_CRAMS_MAX) {
			printf("PMD: Bad CRAM number %d",cram) ;
			return 0 ;
		}

		if((blk != 0) && (blk != 1)) {
			printf("PMD: Bad Block %d in CRAM %d",blk,cram) ;
			return 0 ;
		}


		for(channel=0;channel<ch;channel++) {
			unsigned int val ;

			val = (*datum) ;
			datum++ ;


			//printf("        %4d == %4d (0x%04X)\n",channel,val,val) ;

			switch(type) {
			case PMD_PEDR_N :
				mThePmd.ped[sec][cram][blk][channel] = val;
				break ;
			case PMD_RMSR_N :
				mThePmd.rms[sec][cram][blk][channel] = val ;
				break ;
			case PMD_THRR_N :
				mThePmd.thr[sec][cram][blk][channel] = val ;
				break ;
			}
			ch_num++ ;
					
		}
	}

	return ch_num ;
}
