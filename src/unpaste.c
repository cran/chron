#include <R.h>
#include <Rinternals.h>

#define BUF_SIZ 4096

void
unpaste(char **strings, Sint *nstrings, char **sep, Sint *whitespace,
	Sint *nfields, SEXP *output)
{
    Sint i, j, k;
    char *s, buffer[BUF_SIZ];
    int	c;

    /* allocate character vectors for each field */
    for(k = 0; k < *nfields; ++k)
	PROTECT(output[k] = allocVector(STRSXP, *nstrings));

    for(i = 0; i < *nstrings; ++i) {
	s = strings[i];
	if(*whitespace)		/* skip initial whitespace */
	    while(isspace(*s)) ++s;
	j = k = 0;
	while(1) {
	    c = *s;
	    if((c == '\0')
	       || (*whitespace && isspace(c))
	       || (!*whitespace && c == **sep)) {
		buffer[j++] = '\0';
#if R_VERSION >= R_Version(1, 2, 0)
		SET_STRING_ELT(output[k], i, allocString(j));
		strcpy(CHAR(STRING_ELT(output[k], i)), buffer);
#else
		STRING(output[k])[i] = allocString(j);
		strcpy(CHAR(STRING(output[k])[i]), buffer);
#endif
		if(c=='\0')
		    break;
		k++;
		j = 0;
		if(*whitespace) {
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
    UNPROTECT(*nfields);
}
