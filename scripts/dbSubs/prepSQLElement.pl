#!/usr/bin/perl -w
#
# $Id: prepSQLElement.pl,v 1.1 2003/01/10 22:56:55 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: creates column definition (SQL) for a element
#  
#****************************************************************************
# 
# $Log: prepSQLElement.pl,v $
# Revision 1.1  2003/01/10 22:56:55  porter
# new scripts for db auto table generation
#
#
#
###################################################


sub prepSQLElement {

%mysqlTypes = (
               float  => "float",
               double => "double",
               int    => "int",
               uint   => "int unsigned",
               uchar  => " tinyint unsigned",
               short  => " smallint",
               ushort => " smallint unsigned"
               );


    my ($sname, $stype, $sstore, $slength, $sposition) = @_;  
#    print $sname," ",$stype," ",$sstore," ",$slength," ",$sposition,"\n";

    my $tmpType = $mysqlTypes{$stype};

    if ($stype=~m/uchar/){
        $tmpType="mediumtext";
        if ($slength=~m/\,/ || $slength>60){ #multi-dim or large array
          $tmpType="longblob";
        } 
    } elsif ($stype=~m/char/){
        if($slength<=60){
            $tmpType="char(".$slength.")";
        } elsif($slength<=255){
            $tmpType="varchar(".$slength.")";
        } else {
            $tmpType="mediumtext";
        }            
    } elsif ($slength=~m/\,/ || $slength>60){ #multi-dim or large array
        $tmpType="longblob";
    } elsif ($slength>1){
        $tmpType="mediumtext"; # small array
    } else {
        $tmpType=~s/float/float \(16\,8\)/;
        $tmpType=~s/double/double \(20\,10\)/;
    }

   if($tmpType=~m/blob/){
      ($sstore=~m/bin/) 
       || die "FATAL :: Blob storetype not binary for element $sname\n";
   }
    
    my $retVal= qq{ $sname $tmpType };
    return $retVal;
}

1;



