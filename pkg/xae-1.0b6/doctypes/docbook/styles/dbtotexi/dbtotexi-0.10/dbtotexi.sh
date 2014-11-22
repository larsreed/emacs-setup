#!DBTOTEXISHELL

function usage () {
  echo "usage: dbtotexi xmlfile [texinfofile [infofile]]"
}

if [ $# -lt 1 ]; then
  usage
  exit 1
fi

if [ "x$1" = "x--help" ]; then
  usage
  exit 0
fi

if [ "x$1" = "x--version" ]; then
  echo "GNU dbtotexi VERSION"
  exit 0
fi

xpjar=${DBTOTEXIHOME:=DEFAULTDBTOTEXIHOME}/xp.jar
xtjar=$DBTOTEXIHOME/xt.jar
saxjar=$DBTOTEXIHOME/sax.jar
appclasspath=$DBTOTEXIHOME:$xpjar:$xtjar:$saxjar

if [ $# -gt 1 ]; then
  if [ "x$2" = "x-" ]; then
    out=
  else
    out=$2
  fi
else
  out=`dirname $1`/`basename $1 .xml`.texinfo
fi

if [ $# -gt 2 ]; then
  infofile=$3
else
  infofile=`dirname $1`/`basename $1 .xml`.info
fi

JAVAVM com.jclark.xsl.sax.Driver $1 $DBTOTEXIHOME/dbtotexi.xsl $out infofilename=$infofile
