SASH=${SASH:-"sagittarius"}
GOSH=${GOSH:-"gosh"}
# not properly supported though
CHIBI=${CHIBI:-"chibi-scheme"}

test_sagittarius()
{
    $SASH -Llib -S.sld test.scm
}

test_gauche()
{
    $GOSH -r7 -e '(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
	-e "(append! *load-path* (list \"lib\" \".\"))" test.scm
}

test_chibi()
{
    $CHIBI -Ilib test.scm
}

usage()
{
    echo "usage: $0 sagittarius|gauche|chibi"
}

if [ $# -ge 1 ] ; then
    name=$1
    shift
    case $name in
	sagittarius) test_sagittarius;;
	gauche)      test_gauche;;
	chibi)       test_chibi;;
	*)           usage;;
    esac
else
    usage
fi
