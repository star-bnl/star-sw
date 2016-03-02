#!/opt/star/bin/perl -w
#
# $Id: parseXmlNode.pl,v 1.2 2001/02/16 22:11:38 porter Exp $
#
# Author: R. Jeff Porter
#
#***************************************************************************
#
# Description: parses an Xml-Node description & loads into memory
#
#****************************************************************************
# 
# $Log: parseXmlNode.pl,v $
# Revision 1.2  2001/02/16 22:11:38  porter
# modified for new low-level table structures
#
# Revision 1.1  2000/04/28 14:08:22  porter
# management perl scripts for db-structure accessible from StDbLib
#
#
#####################################

use XML::Parser;

sub parse_Nodes {

  my %args = (
              DEBUG => '',
              fileName => '',
              @_,
              );

  if($args{DEBUG}){
  print $args{fileName}, "\n";
}

  $filename = $args{fileName};

%colormap = (
             directory => "navy",
             file => "forestgreen"
             );


$depth = 0;
$fullpath = '';
@currentElement = '';
$content=" ";
$p1 = new XML::Parser(Handlers => {Start => \&handle_start,
                                     End   => \&handle_end,
                                   Char  => \&handle_char});

#  $content='';


$p1->parsefile($filename);

  return ;
}

######################
sub handle_start {
    local ($expat, $element) = @_;
    $depth = $depth +1;
    $element =~ s/[\s\n]*//g;
    $currentElement = $element;
}

######################
sub handle_end {
    local ($expat, $element) = @_;
    $depth = $depth-1;
}

######################
sub handle_char {
# Here's where the variables are loaded Keyed by "currentElement"
   local ($expat, $element) = @_;
   if(!($currentElement eq 'comment')){ $element =~ s/[\s\n]*//g; }

    if ( $element ne '' ) {
        if ( $currentElement eq 'StarDataBase' ) {
            $dbName=$element;
        }
        if ( $currentElement eq 'dbNode') {
            $#nodeName++;
            $i=$#nodeVersion=$#nodeType=$#nodeStruct=$#nodeElementIDs=$#nodeBaseline=$#nodeIndexed=$#nodeBinary=$#nodeComment=$#nodeName;
            $nodeName[$i]=$element;
            $nodeType[$i]="directory";
            $nodeStruct[$i]="None";
            $nodeBaseline[$i]="N";
            $nodeIndexed[$i]="Y";
            $nodeBinary[$i]="N";
            $nodeElementIDs[$i]="None";
            $nodeVersion[$i]="default";
            $nodeComment[$i]='';
            $nodeIndexName[$i]="None";
            $nodeIndexVal[$i]=0;
        }

        if ( $currentElement eq 'StDbTable') {
            $nodeStruct[$i]=$element;
            $nodeType[$i]="table";
        }

        if($currentElement eq 'baseLine' && $element eq 'Y'){
                $nodeBaseline[$i]=$element;
        }
        if($currentElement eq 'binary' && $element eq 'Y'){
                $nodeBinary[$i]=$element;
        }
        if($currentElement eq 'indexed' && $element eq 'N'){
                $nodeIndexed[$i]=$element;
        }

        if ( $currentElement eq 'version') {
            $nodeVersion[$i]=$element;
        }

        if ( $currentElement eq 'elementID') {
            $nodeElementIDs[$i]=$element;
        }
        if ( $currentElement eq 'comment' && !$nodeComment[$i]) {
            $nodeComment[$i]=$element;
          #  print "ElementComment = ",$element,"\n";
        }
        if ( $currentElement eq 'indexName'){
	    $nodeIndexName[$i]=$element;
	}
        if ( $currentElement eq 'indexVal'){
	    $nodeIndexVal[$i]=$element;
	}
  
        if ( $currentElement eq 'type') {
            $nodeType[$i]=$element;
            $checkIt=0;
            if($nodeType[$i] eq 'Config'){ $checkIt=1;}
            if($nodeType[$i] eq 'directory'){ $checkIt=1;}
            if($nodeType[$i] eq 'DB'){ $checkIt=1;}
            if($nodeType[$i] eq 'table'){ $checkIt=1;}
            if(!$checkIt) { 
                $nodeType[$i]="directory";
                print "WARNING:: type=",$element," not allowed, set to default type (directory) \n";
            }
       }
 
    }
        
}

1;
















