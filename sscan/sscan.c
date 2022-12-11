// 1985 Feb 08 14:05
/* %Z% %M% %I% %H% %T% */
#include <stdio.h>
#include <filehdr.h>
#include "ldfcn.h"
#include <syms.h>

char* ldgetname(LDFILE*,SYMENT*) ;

typedef  char* Strptr;
Strptr copy( Strptr ) ;

int err = 0 ;

class Strings {
    Strptr* argv;
    int first ;
    int bound ;
    void check(int);

    public:
    int len ;
    Strptr& operator[] (int x) { check(x) ; return argv[first+x] ; } ;
    void suffix_copy(Strptr s) {
        check(first+len+1) ; argv[first+(len++)] = copy(s) ; } ;
    Strings( int x = 32 ) {
       argv = new Strptr[x+1] ; first = x/2 ; len = 0 ; bound = x ;
       } ;
    } ;

void Strings.check(int want) {
    if ( want <= 0 || want >= bound ) {
        int new_bound = 3*(len+1) ;
        int new_first = new_bound/3 ;
        Strptr* new_argv = new Strptr[new_bound+1] ;
        for ( int x = 0 ; x < len ; ++x ) {
            new_argv[new_first+x] = argv[first+x] ;
            }
        delete argv ;
        first = new_first ;
        bound = new_bound ;
        argv = new_argv ;
        }
    }

Strptr copy ( Strptr old ) {
    Strptr new_s = new char[strlen(old)+1] ;
    strcpy(new_s,old) ;
    return new_s ;
    }

Strings* cons ;

Strings* dest ;

void dofile( char* n ) {
    LDFILE* f = ldopen(n,0) ;
    if ( f == 0 ) {
        char buffer[BUFSIZ] ;
        sprintf(buffer,"sscan(%s)", n) ;
        perror(buffer) ;
        err = 4 ;
        return ;
        }
    while (f != 0 ) {
        SYMENT sym;
        ldtbseek( f ) ;
        for ( int x_sym = 0 ; ldtbread(f,x_sym,&sym) == SUCCESS ; ++x_sym ) {
            char* str = ldgetname(f,&sym);
            if ( strcmp(str,"_STI",4) == 0 ) {
                cons->suffix_copy(str) ;
                }
            else if ( strncmp(str,"_STD",4) == 0 ) {
                dest->suffix_copy(str) ;
                }
            x_sym += sym.n_numaux ; /* skip auxentries */
            }
        if ( ldclose(f) != FAILURE ) f = 0 ;
        }
    }

main(int argc, char** argv) {
    int monitor = 0 ;
    cons = new Strings ;
    dest = new Strings ;

    for ( int x_arg = 1 ; x_arg < argc ; ++ x_arg ) {
        int len = strlen(argv[x_arg]) ;
        if ( len>=3 && argv[x_arg][0] != '-'
                && argv[x_arg][len-2] == '.'
                && strchr("oa",argv[x_arg][len-1]) != 0  ) {
           dofile( argv[x_arg] ) ;
           }
        else if ( strcmp(argv[x_arg],"-p") == 0 ) monitor=1 ;
        }
    printf("void _STC_all(e) int e ; {\n" ) ;
    for ( x_arg = 0 ; x_arg < cons->len ; ++x_arg ) {
        printf("%s();\n", (*cons)[x_arg] ) ;
        }
    printf("}\n");
    printf("void exit(err) {\n" ) ;
    for ( x_arg = 0 ; x_arg < dest->len ; ++x_arg ) {
        printf("%s();\n", (*dest)[x_arg] ) ;
        }
    if ( monitor ) printf("monitor();\n");
    printf("_cleanup();\n");
    printf("_exit(err);}\n");
    return err ;
    }
