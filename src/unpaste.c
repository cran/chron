#include "Rchron.h"

#include <Rdefines.h>

#define BUF_SIZ 4096

SEXP
unpaste(SEXP s_strings, SEXP s_sep, SEXP s_whitespace,
	SEXP s_nfields)
{
    Sint i, j, k;
    char *s, buffer[BUF_SIZ];
    int	c;
    SEXP *output, ans;
    int nstrings = Rf_length(s_strings);
    char *sep = CHAR(STRING_ELT(s_sep, 0));
    int nfields = INTEGER(s_nfields)[0];
    Rboolean whitespace = asLogical(s_whitespace);

    if(whitespace == NA_LOGICAL)
	whitespace = FALSE;

    /* allocate character vectors for each field */
    PROTECT(ans = allocVector(VECSXP, nfields));
    output = (SEXP *) S_alloc(nfields, sizeof(SEXP));
    for(k = 0; k < nfields; ++k)
	SET_VECTOR_ELT(ans, k, output[k] = allocVector(STRSXP, nstrings));

    for(i = 0; i < nstrings; ++i) {
	s = CHAR(STRING_ELT(s_strings, i));
	if(whitespace)		/* skip initial whitespace */
	    while(isspace(*s)) ++s;
	j = k = 0;
	while(1) {
 	    if(j >= 4095) {
		    fprintf(stderr, "Problems coming\n");fflush(stderr);
	    }
	    c = *s;
	    if((c == '\0')
	       || (whitespace && isspace(c))
	       || (!whitespace && c == *sep)) {
		buffer[j++] = '\0';
		SET_STRING_ELT(output[k], i, COPY_TO_USER_STRING(buffer));
		if(c=='\0')
		    break;
		k++;
		j = 0;
		if(whitespace) {
		    /* skip trailing space in current item */
		    while(isspace(*(s+1)))
			++s;
		    if(*(s+1) == '\0')
			break;
		}
	    }
	    else
		buffer[j++] = c;
	    ++s;
	}
    }
    UNPROTECT(1);
    return(ans);
}
