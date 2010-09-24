/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
#include "TxEventLogFactory.h"
/**
 *
 * @author Valeri Fine (from  lbhajdu's Java version)
 */

    /**
     * valid input formats can be:
     *
     *      -key [value] -value [value] //Note: as of yet this one can not have spaces
     *      [value]=[value]
     *      [value] = [value]
     *      [value] =[value]
     *      [value]= [value]
     *      [value]     =[value]
     *      [value]=     [value]
     *
     * @param args the command line arguments
     */
     
//________________________________________________________________________
int  Main(int argc, const char *argv[])
{   return TxLogging::TxEventLogFactory::main(argc,argv); }
